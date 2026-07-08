use super::*;

impl Vm {
    fn finish_current_frame(
        &mut self,
        frames: &mut Vec<VmFrame>,
        local_stack: &mut Vec<Value>,
        register_stack: &mut Vec<Value>,
        flow: VmFlow,
    ) -> Result<Option<VmFlow>, Error> {
        let frame = frames.pop().expect("frame must exist");
        sync_frame_locals_to_env_if_needed(&mut self.heap, &frame, local_stack);
        local_stack.truncate(frame.local_base);
        register_stack.truncate(frame.register_base);
        if frames.is_empty() {
            return Ok(Some(flow));
        }
        self.call_stack.pop();
        let value = match flow {
            VmFlow::Value(value) | VmFlow::Return(value) => user_value(value),
            VmFlow::Break(span) => Value::Break(span),
            VmFlow::Continue(span) => Value::Continue(span),
        };
        let parent = frames.last_mut().expect("caller frame must exist");
        if let Some(return_register) = frame.return_register {
            set_register(
                parent,
                register_stack,
                return_register,
                value.clone(),
                zero_span(),
            )?;
        }
        parent.last = value;
        refresh_captured_frame_locals(&self.heap, parent, local_stack);
        Ok(None)
    }

    pub(super) fn execute_register_function(
        &mut self,
        function: Rc<CompiledFunction>,
        env: VmEnvHandle,
    ) -> Result<VmFlow, Error> {
        let result = self.execute_register_function_inner(function, env);
        self.finish_profile_instruction();
        result.map_err(|err| {
            err.with_source_path(self.current_source_path())
                .with_stack(&self.call_stack)
        })
    }

    fn execute_register_function_inner(
        &mut self,
        function: Rc<CompiledFunction>,
        env: VmEnvHandle,
    ) -> Result<VmFlow, Error> {
        let initial_locals = self.heap.env(env).slots.clone();
        let mut local_stack = initial_locals;
        let mut register_stack = vec![Value::Empty; function.registers.register_count];
        let mut frames = vec![VmFrame {
            register_base: 0,
            register_count: function.registers.register_count,
            local_base: 0,
            local_count: local_stack.len(),
            locals_synced: true,
            env,
            function,
            upvalues: Rc::from([]),
            ip: 0,
            return_register: None,
            last: Value::Empty,
        }];
        let mut for_in_stack = Vec::<ForInState>::new();

        loop {
            let frame_index = frames.len() - 1;
            if frames[frame_index].ip >= frames[frame_index].function.registers.instructions.len() {
                let frame = frames.pop().expect("frame must exist");
                sync_frame_locals_to_env_if_needed(&mut self.heap, &frame, &local_stack);
                let value = frame.last;
                local_stack.truncate(frame.local_base);
                register_stack.truncate(frame.register_base);
                let Some(parent) = frames.last_mut() else {
                    return Ok(VmFlow::Value(value));
                };
                self.call_stack.pop();
                let value = user_value(value);
                if let Some(return_register) = frame.return_register {
                    set_register(
                        parent,
                        &mut register_stack,
                        return_register,
                        value.clone(),
                        zero_span(),
                    )?;
                }
                parent.last = value;
                refresh_captured_frame_locals(&self.heap, parent, &mut local_stack);
                continue;
            }

            let ip = frames[frame_index].ip;
            let instruction_ptr = {
                let frame = &frames[frame_index];
                // SAFETY: The end-of-frame check above guarantees ip is within
                // the current function's instruction slice. The instruction
                // storage is owned by the compiled function, not the frame vec,
                // so mutating frames below cannot invalidate this pointer.
                unsafe { frame.function.registers.instructions.as_ptr().add(ip) }
            };
            // SAFETY: instruction_ptr was derived from a bounds-checked ip above.
            let instruction_ref = unsafe { &*instruction_ptr };
            if self.profile.is_some() {
                self.finish_profile_instruction();
                self.record_instruction(
                    instruction_ref.kind(),
                    frames.len(),
                    frames[frame_index].register_count,
                );
            }
            match instruction_ref {
                RegisterInstruction::Jump { target } => {
                    frames[frame_index].ip = *target;
                    continue;
                }
                RegisterInstruction::JumpIfLocalIntFalse {
                    slot,
                    token,
                    operand,
                    target,
                    span,
                } => {
                    if !compare_frame_local_int_bool(
                        &mut self.heap,
                        frame_locals(&frames[frame_index], &local_stack),
                        *slot,
                        token,
                        *operand,
                        *span,
                    )? {
                        frames[frame_index].ip = *target;
                        continue;
                    }
                    frames[frame_index].ip += 1;
                    continue;
                }
                RegisterInstruction::JumpIfLocalLocalFalse {
                    left_slot,
                    token,
                    right_slot,
                    target,
                    span,
                } => {
                    if !compare_frame_local_local_bool(
                        &mut self.heap,
                        frame_locals(&frames[frame_index], &local_stack),
                        *left_slot,
                        token,
                        *right_slot,
                        *span,
                    )? {
                        frames[frame_index].ip = *target;
                        continue;
                    }
                    frames[frame_index].ip += 1;
                    continue;
                }
                RegisterInstruction::UpdateLocalInt {
                    dst,
                    slot,
                    token,
                    operand,
                    span,
                } => {
                    if let Some(dst) = *dst {
                        let value = update_frame_local_int(
                            &mut self.heap,
                            &mut frames[frame_index],
                            &mut local_stack,
                            *slot,
                            token,
                            *operand,
                            *span,
                        )?;
                        set_register_last(
                            &mut frames[frame_index],
                            &mut register_stack,
                            dst,
                            value,
                            *span,
                        )?;
                    } else {
                        update_frame_local_int_no_result(
                            &mut self.heap,
                            &mut frames[frame_index],
                            &mut local_stack,
                            *slot,
                            token,
                            *operand,
                            *span,
                        )?;
                    }
                    frames[frame_index].ip += 1;
                    continue;
                }
                RegisterInstruction::UpdateLocalFromLocal {
                    dst,
                    slot,
                    token,
                    operand_slot,
                    span,
                } => {
                    if let Some(dst) = *dst {
                        let value = update_frame_local_from_local(
                            &mut self.heap,
                            &mut frames[frame_index],
                            &mut local_stack,
                            *slot,
                            token,
                            *operand_slot,
                            *span,
                        )?;
                        set_register_last(
                            &mut frames[frame_index],
                            &mut register_stack,
                            dst,
                            value,
                            *span,
                        )?;
                    } else {
                        update_frame_local_from_local_no_result(
                            &mut self.heap,
                            &mut frames[frame_index],
                            &mut local_stack,
                            *slot,
                            token,
                            *operand_slot,
                            *span,
                        )?;
                    }
                    frames[frame_index].ip += 1;
                    continue;
                }
                _ => {}
            }
            // SAFETY: instruction_ptr was derived from a bounds-checked ip above.
            let instruction = unsafe { (*instruction_ptr).clone() };
            macro_rules! current_env {
                () => {
                    frames[frame_index].env
                };
            }

            match instruction {
                RegisterInstruction::Move { dst, src } => {
                    let value =
                        get_register(&frames[frame_index], &register_stack, src, zero_span())?;
                    set_register(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        zero_span(),
                    )?;
                }
                RegisterInstruction::ConstantInt { dst, value } => {
                    set_register_last(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        Value::Integer(value),
                        zero_span(),
                    )?;
                }
                RegisterInstruction::ConstantFloat { dst, value } => {
                    set_register_last(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        Value::Float(value),
                        zero_span(),
                    )?;
                }
                RegisterInstruction::ConstantBool { dst, value } => {
                    set_register_last(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        Value::Boolean(value),
                        zero_span(),
                    )?;
                }
                RegisterInstruction::ConstantString { dst, constant } => {
                    let value = frames[frame_index]
                        .function
                        .constants
                        .get(constant)
                        .cloned()
                        .ok_or_else(|| {
                            Error::with_kind_span(
                                ErrorKind::Runtime,
                                "vm constant out of bound".to_string(),
                                zero_span(),
                            )
                        })?;
                    let value = self.heap.alloc_string(value);
                    set_register_last(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        zero_span(),
                    )?;
                }
                RegisterInstruction::ConstantNull { dst } => {
                    set_register_last(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        Value::Null,
                        zero_span(),
                    )?;
                }
                RegisterInstruction::ConstantEmpty { dst } => {
                    set_register_last(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        Value::Empty,
                        zero_span(),
                    )?;
                }
                RegisterInstruction::MakeFunction { dst, function } => {
                    sync_frame_locals_to_env_if_dirty(
                        &mut self.heap,
                        &mut frames[frame_index],
                        &local_stack,
                    );
                    mark_env_chain_captured(&mut self.heap, current_env!());
                    let upvalues = capture_vm_upvalues(
                        &mut self.heap,
                        current_env!(),
                        &function.upvalues,
                        zero_span(),
                    )?;
                    self.record_allocation("closure");
                    let value = self.heap.alloc_function(VmFunction {
                        compiled: Rc::clone(&function),
                        env: current_env!(),
                        upvalues,
                    });
                    set_register_last(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        zero_span(),
                    )?;
                }
                RegisterInstruction::GetName { dst, name, span } => {
                    let value = self.get_name(current_env!(), name, span, (0, ip))?;
                    set_register_last(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::GetLocal { dst, slot, span } => {
                    let value = get_frame_local(&frames[frame_index], &local_stack, slot, span)?;
                    set_register_last(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::GetSlot {
                    dst,
                    depth,
                    slot,
                    span,
                } => {
                    let value = get_vm_slot(&self.heap, current_env!(), depth, slot, span)?;
                    set_register_last(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::GetUpvalue { dst, index, span } => {
                    let value = frames[frame_index]
                        .upvalues
                        .get(index)
                        .map(|cell| self.heap.cell(*cell))
                        .ok_or_else(|| {
                            Error::with_kind_span(
                                ErrorKind::Runtime,
                                "vm upvalue out of bound".to_string(),
                                span,
                            )
                        })?;
                    set_register_last(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::DefineLocal {
                    src,
                    name,
                    slot,
                    span,
                } => {
                    let value = get_register(&frames[frame_index], &register_stack, src, span)?;
                    if has_current_vm_binding(&self.heap, current_env!(), name) {
                        let name = self.symbol_name(name);
                        return Err(Error::with_kind_span(
                            ErrorKind::Name,
                            format!("cannot redeclare name '{name}' in the same scope"),
                            span,
                        ));
                    }
                    define_vm_slot_binding(&mut self.heap, current_env!(), name, slot);
                    set_frame_local(
                        &mut self.heap,
                        &mut frames[frame_index],
                        &mut local_stack,
                        slot,
                        value,
                        span,
                    )?;
                    frames[frame_index].last = Value::Empty;
                }
                RegisterInstruction::DefineLocalConst {
                    name,
                    slot,
                    value,
                    span,
                } => {
                    if has_current_vm_binding(&self.heap, current_env!(), name) {
                        let name = self.symbol_name(name);
                        return Err(Error::with_kind_span(
                            ErrorKind::Name,
                            format!("cannot redeclare name '{name}' in the same scope"),
                            span,
                        ));
                    }
                    define_vm_slot_binding(&mut self.heap, current_env!(), name, slot);
                    let value = register_constant_to_value(value);
                    set_frame_local(
                        &mut self.heap,
                        &mut frames[frame_index],
                        &mut local_stack,
                        slot,
                        value,
                        span,
                    )?;
                    frames[frame_index].last = Value::Empty;
                }
                RegisterInstruction::SetName {
                    dst,
                    src,
                    name,
                    span,
                } => {
                    let value = get_register(&frames[frame_index], &register_stack, src, span)?;
                    self.set_name(current_env!(), name, value.clone(), span, (0, ip))?;
                    set_optional_register_or_empty(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::SetLocal {
                    dst,
                    src,
                    slot,
                    span,
                } => {
                    let value = get_register(&frames[frame_index], &register_stack, src, span)?;
                    set_frame_local(
                        &mut self.heap,
                        &mut frames[frame_index],
                        &mut local_stack,
                        slot,
                        value.clone(),
                        span,
                    )?;
                    set_optional_register_or_empty(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::SetLocalConst {
                    dst,
                    slot,
                    value,
                    span,
                } => {
                    let value = register_constant_to_value(value);
                    set_frame_local(
                        &mut self.heap,
                        &mut frames[frame_index],
                        &mut local_stack,
                        slot,
                        value.clone(),
                        span,
                    )?;
                    set_optional_register_or_empty(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::SetSlot {
                    dst,
                    src,
                    depth,
                    slot,
                    span,
                } => {
                    let value = get_register(&frames[frame_index], &register_stack, src, span)?;
                    set_vm_slot(
                        &mut self.heap,
                        current_env!(),
                        depth,
                        slot,
                        value.clone(),
                        span,
                    )?;
                    set_optional_register_or_empty(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::SetUpvalue {
                    dst,
                    src,
                    index,
                    span,
                } => {
                    let value = get_register(&frames[frame_index], &register_stack, src, span)?;
                    let cell = frames[frame_index]
                        .upvalues
                        .get(index)
                        .copied()
                        .ok_or_else(|| {
                            Error::with_kind_span(
                                ErrorKind::Runtime,
                                "vm upvalue out of bound".to_string(),
                                span,
                            )
                        })?;
                    self.heap.set_cell(cell, value.clone());
                    set_optional_register_or_empty(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::UpdateNameInfix {
                    dst,
                    name,
                    right,
                    token,
                    span,
                } => {
                    let right = get_register(&frames[frame_index], &register_stack, right, span)?;
                    let value =
                        self.update_name_infix(current_env!(), name, &token, right, span, (0, ip))?;
                    set_optional_register_or_empty(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::UpdateLocalInfix {
                    dst,
                    slot,
                    right,
                    token,
                    span,
                } => {
                    let right = get_register(&frames[frame_index], &register_stack, right, span)?;
                    let left = get_frame_local(&frames[frame_index], &local_stack, slot, span)?;
                    let value = eval_infix_value(&mut self.heap, &token, left, right, span)?;
                    set_frame_local(
                        &mut self.heap,
                        &mut frames[frame_index],
                        &mut local_stack,
                        slot,
                        value.clone(),
                        span,
                    )?;
                    set_optional_register_or_empty(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::UpdateLocalInt {
                    dst,
                    slot,
                    token,
                    operand,
                    span,
                } => {
                    let value = update_frame_local_int(
                        &mut self.heap,
                        &mut frames[frame_index],
                        &mut local_stack,
                        slot,
                        &token,
                        operand,
                        span,
                    )?;
                    set_optional_register_or_empty(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::UpdateLocalFromLocal {
                    dst,
                    slot,
                    token,
                    operand_slot,
                    span,
                } => {
                    let value = update_frame_local_from_local(
                        &mut self.heap,
                        &mut frames[frame_index],
                        &mut local_stack,
                        slot,
                        &token,
                        operand_slot,
                        span,
                    )?;
                    set_optional_register_or_empty(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::UpdateSlotInfix {
                    dst,
                    depth,
                    slot,
                    right,
                    token,
                    span,
                } => {
                    let right = get_register(&frames[frame_index], &register_stack, right, span)?;
                    if let Some(dst) = dst {
                        let value = eval_vm_slot_infix(
                            &mut self.heap,
                            current_env!(),
                            depth,
                            slot,
                            &token,
                            right,
                            span,
                        )?;
                        set_vm_slot(
                            &mut self.heap,
                            current_env!(),
                            depth,
                            slot,
                            value.clone(),
                            span,
                        )?;
                        set_register_last(
                            &mut frames[frame_index],
                            &mut register_stack,
                            dst,
                            value,
                            span,
                        )?;
                    } else {
                        let value = eval_vm_slot_infix(
                            &mut self.heap,
                            current_env!(),
                            depth,
                            slot,
                            &token,
                            right,
                            span,
                        )?;
                        set_vm_slot(&mut self.heap, current_env!(), depth, slot, value, span)?;
                        frames[frame_index].last = Value::Empty;
                    }
                }
                RegisterInstruction::UpdateUpvalueInfix {
                    dst,
                    index,
                    right,
                    token,
                    span,
                } => {
                    let right = get_register(&frames[frame_index], &register_stack, right, span)?;
                    let cell = frames[frame_index]
                        .upvalues
                        .get(index)
                        .copied()
                        .ok_or_else(|| {
                            Error::with_kind_span(
                                ErrorKind::Runtime,
                                "vm upvalue out of bound".to_string(),
                                span,
                            )
                        })?;
                    let left = self.heap.cell(cell);
                    let value = eval_infix_value(&mut self.heap, &token, left, right, span)?;
                    self.heap.set_cell(cell, value.clone());
                    set_optional_register_or_empty(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::UpdateUpvalueInt {
                    dst,
                    index,
                    token,
                    operand,
                    span,
                } => {
                    let cell = frames[frame_index]
                        .upvalues
                        .get(index)
                        .copied()
                        .ok_or_else(|| {
                            Error::with_kind_span(
                                ErrorKind::Runtime,
                                "vm upvalue out of bound".to_string(),
                                span,
                            )
                        })?;
                    let left = self.heap.cell(cell);
                    let value = if let Value::Integer(left) = left {
                        eval_register_int_int_infix(left, &token, operand, span)?
                    } else {
                        eval_infix_value(
                            &mut self.heap,
                            &token,
                            left,
                            Value::Integer(operand),
                            span,
                        )?
                    };
                    self.heap.set_cell(cell, value.clone());
                    set_optional_register_or_empty(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::UpdateUpvalueFromUpvalue {
                    dst,
                    index,
                    token,
                    operand_index,
                    span,
                } => {
                    let cell = frames[frame_index]
                        .upvalues
                        .get(index)
                        .copied()
                        .ok_or_else(|| {
                            Error::with_kind_span(
                                ErrorKind::Runtime,
                                "vm upvalue out of bound".to_string(),
                                span,
                            )
                        })?;
                    let operand_cell = frames[frame_index]
                        .upvalues
                        .get(operand_index)
                        .copied()
                        .ok_or_else(|| {
                            Error::with_kind_span(
                                ErrorKind::Runtime,
                                "vm upvalue out of bound".to_string(),
                                span,
                            )
                        })?;
                    let left = self.heap.cell(cell);
                    let right = self.heap.cell(operand_cell);
                    let value =
                        if let (Value::Integer(left), Value::Integer(right)) = (&left, &right) {
                            eval_register_int_int_infix(*left, &token, *right, span)?
                        } else {
                            eval_infix_value(&mut self.heap, &token, left, right, span)?
                        };
                    self.heap.set_cell(cell, value.clone());
                    set_optional_register_or_empty(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::Pop { src } => {
                    if let Some(src) = src {
                        let _ =
                            get_register(&frames[frame_index], &register_stack, src, zero_span())?;
                    }
                    frames[frame_index].last = Value::Empty;
                }
                RegisterInstruction::AssertBool { dst, src, span } => {
                    let value = get_register(&frames[frame_index], &register_stack, src, span)?;
                    let value = bool_condition(value, span)?;
                    set_register_last(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        Value::Boolean(value),
                        span,
                    )?;
                }
                RegisterInstruction::Prefix {
                    dst,
                    src,
                    token,
                    span,
                } => {
                    let value = get_register(&frames[frame_index], &register_stack, src, span)?;
                    let value = eval_prefix_value(&token, value, span)?;
                    set_register_last(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::Infix {
                    dst,
                    left,
                    right,
                    token,
                    span,
                } => {
                    let left = get_register(&frames[frame_index], &register_stack, left, span)?;
                    let right = get_register(&frames[frame_index], &register_stack, right, span)?;
                    let value = eval_infix_value(&mut self.heap, &token, left, right, span)?;
                    set_register_last(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::InfixLocalInt {
                    dst,
                    slot,
                    token,
                    operand,
                    span,
                } => {
                    let value = eval_local_int_infix_from_values(
                        &mut self.heap,
                        frame_locals(&frames[frame_index], &local_stack),
                        slot,
                        &token,
                        operand,
                        span,
                    )?;
                    set_register_last(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::InfixLocalLocal {
                    dst,
                    left_slot,
                    token,
                    right_slot,
                    span,
                } => {
                    let left =
                        get_frame_local(&frames[frame_index], &local_stack, left_slot, span)?;
                    let right =
                        get_frame_local(&frames[frame_index], &local_stack, right_slot, span)?;
                    let value = eval_infix_value(&mut self.heap, &token, left, right, span)?;
                    set_register_last(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::CompareLocalInt {
                    dst,
                    slot,
                    token,
                    operand,
                    span,
                } => {
                    let value = compare_frame_local_int(
                        &mut self.heap,
                        frame_locals(&frames[frame_index], &local_stack),
                        slot,
                        &token,
                        operand,
                        span,
                    )?;
                    set_register_last(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::MakeSlice { dst, elements } => {
                    let values = elements
                        .iter()
                        .map(|register| {
                            get_register(
                                &frames[frame_index],
                                &register_stack,
                                *register,
                                zero_span(),
                            )
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    self.record_allocation("list");
                    let value = self.heap.alloc_list(values);
                    set_register_last(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        zero_span(),
                    )?;
                }
                RegisterInstruction::MakeMap { dst, entries, span } => {
                    let mut map = HashMap::new();
                    for (key, value) in entries {
                        let key = vm_map_key_from_value(
                            get_register(&frames[frame_index], &register_stack, key, span)?,
                            &self.heap,
                            span,
                        )?;
                        let value =
                            get_register(&frames[frame_index], &register_stack, value, span)?;
                        map.insert(key, value);
                    }
                    self.record_allocation("map");
                    let value = self.heap.alloc_map(map);
                    set_register_last(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::Index {
                    dst,
                    left,
                    index,
                    span,
                } => {
                    let left = get_register(&frames[frame_index], &register_stack, left, span)?;
                    let index = get_register(&frames[frame_index], &register_stack, index, span)?;
                    let value = eval_index(&mut self.heap, left, index, span)?;
                    set_register_last(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::Member {
                    dst,
                    left,
                    property,
                    span,
                } => {
                    let left = get_register(&frames[frame_index], &register_stack, left, span)?;
                    let interner = self.interner.borrow();
                    let property_name = interner.name(property);
                    let function_id = Rc::as_ptr(&frames[frame_index].function) as usize;
                    let property_key = self
                        .member_key_cache
                        .entry((function_id, ip))
                        .or_insert_with(|| property_name.into());
                    let value = eval_member_value_with_key(
                        &self.heap,
                        left,
                        property,
                        property_name,
                        property_key,
                        span,
                    )?;
                    set_register_last(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::SetIndex {
                    dst,
                    left,
                    index,
                    value,
                    span,
                } => {
                    let left = get_register(&frames[frame_index], &register_stack, left, span)?;
                    let index = get_register(&frames[frame_index], &register_stack, index, span)?;
                    let value = get_register(&frames[frame_index], &register_stack, value, span)?;
                    set_index(&mut self.heap, left, index, value.clone(), span)?;
                    set_optional_register_or_empty(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::SetMember {
                    dst,
                    left,
                    value,
                    property,
                    span,
                } => {
                    let left = get_register(&frames[frame_index], &register_stack, left, span)?;
                    let value = get_register(&frames[frame_index], &register_stack, value, span)?;
                    let interner = self.interner.borrow();
                    let property_name = interner.name(property);
                    set_member(&mut self.heap, left, property_name, value.clone(), span)?;
                    set_optional_register_or_empty(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::CallBuiltin {
                    dst,
                    op,
                    args,
                    span,
                } => {
                    let values = collect_builtin_args_from_registers(
                        &frames[frame_index],
                        &register_stack,
                        args.as_slice(),
                    );
                    let value = self.call_builtin_direct(op, &values, span)?;
                    set_optional_register_or_empty(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::CallBuiltin0 { dst, op, span } => {
                    let value = self.call_builtin_direct(op, &[], span)?;
                    set_optional_register_or_empty(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::CallBuiltin1 {
                    dst,
                    op,
                    arg0,
                    span,
                } => {
                    let values = [read_compiled_register(
                        &frames[frame_index],
                        &register_stack,
                        arg0,
                    )];
                    let value = self.call_builtin_direct(op, &values, span)?;
                    set_optional_register_or_empty(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::CallBuiltin2 {
                    dst,
                    op,
                    arg0,
                    arg1,
                    span,
                } => {
                    let values = [
                        read_compiled_register(&frames[frame_index], &register_stack, arg0),
                        read_compiled_register(&frames[frame_index], &register_stack, arg1),
                    ];
                    let value = self.call_builtin_direct(op, &values, span)?;
                    set_optional_register_or_empty(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::CallBuiltin3 {
                    dst,
                    op,
                    arg0,
                    arg1,
                    arg2,
                    span,
                } => {
                    let values = [
                        read_compiled_register(&frames[frame_index], &register_stack, arg0),
                        read_compiled_register(&frames[frame_index], &register_stack, arg1),
                        read_compiled_register(&frames[frame_index], &register_stack, arg2),
                    ];
                    let value = self.call_builtin_direct(op, &values, span)?;
                    set_optional_register_or_empty(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::AppendLocalNoResult {
                    list_slot,
                    value_slot,
                    span,
                } => {
                    let list =
                        get_frame_local(&frames[frame_index], &local_stack, list_slot, span)?;
                    let value =
                        get_frame_local(&frames[frame_index], &local_stack, value_slot, span)?;
                    match list {
                        Value::Slice(values) => self.heap.list_mut(values).push(value),
                        _ => return Err(operation_type_error(span)),
                    }
                    frames[frame_index].last = Value::Empty;
                }
                RegisterInstruction::DefineStruct {
                    name,
                    slot,
                    fields,
                    span,
                } => {
                    let type_name = self.symbol_name(name);
                    define_vm_slot_binding(&mut self.heap, current_env!(), name, slot);
                    self.record_allocation("struct_type");
                    let value = self.heap.alloc_struct_type(VmStructType {
                        name: type_name,
                        fields,
                    });
                    set_frame_local(
                        &mut self.heap,
                        &mut frames[frame_index],
                        &mut local_stack,
                        slot,
                        value,
                        span,
                    )?;
                    frames[frame_index].last = Value::Empty;
                }
                RegisterInstruction::MakeStructNamed {
                    dst,
                    struct_type,
                    fields,
                    values,
                    span,
                } => {
                    let struct_type_value =
                        get_register(&frames[frame_index], &register_stack, struct_type, span)?;
                    let mut stack = vec![struct_type_value];
                    for register in values {
                        stack.push(get_register(
                            &frames[frame_index],
                            &register_stack,
                            register,
                            span,
                        )?);
                    }
                    let mut values = stack.split_off(1);
                    let struct_type = pop_struct_type(&self.heap, &mut stack, span)?;
                    let interner = self.interner.borrow();
                    let value = make_struct_named(
                        &mut self.heap,
                        struct_type,
                        fields.as_slice(),
                        std::mem::take(&mut values),
                        &interner,
                        span,
                    )?;
                    set_register_last(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::MakeStructPositional {
                    dst,
                    struct_type,
                    values,
                    span,
                } => {
                    let mut stack = vec![get_register(
                        &frames[frame_index],
                        &register_stack,
                        struct_type,
                        span,
                    )?];
                    let mut values = values
                        .iter()
                        .map(|register| {
                            get_register(&frames[frame_index], &register_stack, *register, span)
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    let struct_type = pop_struct_type(&self.heap, &mut stack, span)?;
                    let value = make_struct_positional(
                        &mut self.heap,
                        struct_type,
                        std::mem::take(&mut values),
                        span,
                    )?;
                    set_register_last(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::Import { alias, path, span } => {
                    sync_frame_locals_to_env_if_dirty(
                        &mut self.heap,
                        &mut frames[frame_index],
                        &local_stack,
                    );
                    let module = self.import_module(path.as_str(), span, current_env!())?;
                    if let Some(alias) = alias {
                        define_vm_env(&mut self.heap, current_env!(), alias, module);
                    } else {
                        inject_vm_module(&mut self.heap, current_env!(), &module)?;
                    }
                    refresh_frame_locals_from_env(
                        &self.heap,
                        &mut frames[frame_index],
                        &mut local_stack,
                    );
                    frames[frame_index].last = Value::Empty;
                }
                RegisterInstruction::Export { name, span } => {
                    if !has_current_vm_binding(&self.heap, current_env!(), name) {
                        let name_text = self.symbol_name(name);
                        if let Some(op) = BuiltinOp::from_name(name_text.as_str()) {
                            define_vm_env(&mut self.heap, current_env!(), name, Value::Builtin(op));
                        } else {
                            return Err(Error::with_kind_span(
                                ErrorKind::Name,
                                format!("cannot export undefined name '{name_text}'"),
                                span,
                            ));
                        }
                    }
                    mark_vm_export(&mut self.heap, current_env!(), name);
                    frames[frame_index].last = Value::Empty;
                }
                RegisterInstruction::Jump { target } => {
                    frames[frame_index].ip = target;
                    continue;
                }
                RegisterInstruction::ScopeCleanupJump {
                    target,
                    depth,
                    span,
                } => {
                    leave_register_scope_depth(
                        &mut self.heap,
                        &mut frames[frame_index],
                        &mut local_stack,
                        depth,
                        span,
                    )?;
                    frames[frame_index].ip = target;
                    continue;
                }
                RegisterInstruction::JumpIfFalse { condition, target } => {
                    let condition = get_register(
                        &frames[frame_index],
                        &register_stack,
                        condition,
                        zero_span(),
                    )?;
                    if !bool_condition(condition, zero_span())? {
                        frames[frame_index].ip = target;
                        continue;
                    }
                }
                RegisterInstruction::JumpIfLocalFalse { slot, target, span } => {
                    let condition =
                        get_frame_local(&frames[frame_index], &local_stack, slot, span)?;
                    if !bool_condition(condition, span)? {
                        frames[frame_index].ip = target;
                        continue;
                    }
                }
                RegisterInstruction::JumpIfLocalIntFalse {
                    slot,
                    token,
                    operand,
                    target,
                    span,
                } => {
                    if !compare_frame_local_int_bool(
                        &mut self.heap,
                        frame_locals(&frames[frame_index], &local_stack),
                        slot,
                        &token,
                        operand,
                        span,
                    )? {
                        frames[frame_index].ip = target;
                        continue;
                    }
                }
                RegisterInstruction::JumpIfLocalLocalFalse {
                    left_slot,
                    token,
                    right_slot,
                    target,
                    span,
                } => {
                    let left =
                        get_frame_local(&frames[frame_index], &local_stack, left_slot, span)?;
                    let right =
                        get_frame_local(&frames[frame_index], &local_stack, right_slot, span)?;
                    let result = eval_infix_value(&mut self.heap, &token, left, right, span)?;
                    if !bool_condition(result, span)? {
                        frames[frame_index].ip = target;
                        continue;
                    }
                }
                RegisterInstruction::ForInStart { collection, span } => {
                    sync_frame_locals_to_env_if_dirty(
                        &mut self.heap,
                        &mut frames[frame_index],
                        &local_stack,
                    );
                    let collection =
                        get_register(&frames[frame_index], &register_stack, collection, span)?;
                    let entries = snapshot_for_in_entries(collection, &mut self.heap, span)?;
                    for_in_stack.push(ForInState {
                        entries,
                        index: 0,
                        parent_env: current_env!(),
                    });
                    frames[frame_index].last = Value::Empty;
                }
                RegisterInstruction::ForInNext {
                    key,
                    key_slot,
                    value,
                    local_count,
                    end_target,
                    span,
                } => {
                    let Some(state) = for_in_stack.last_mut() else {
                        return Err(Error::with_kind_span(
                            ErrorKind::Runtime,
                            "for-in state underflow".to_string(),
                            span,
                        ));
                    };
                    if state.index >= state.entries.len() {
                        for_in_stack.pop();
                        frames[frame_index].ip = end_target;
                        continue;
                    }
                    let (entry_key, entry_value) = state.entries[state.index].clone();
                    state.index += 1;
                    let loop_env = VmEnv::alloc_child(&mut self.heap, current_env!(), local_count);
                    let mut locals = vec![Value::Empty; local_count];
                    define_vm_slot_binding(&mut self.heap, loop_env, key, key_slot);
                    locals[key_slot] = entry_key;
                    if let Some((value_name, value_slot)) = value {
                        define_vm_slot_binding(&mut self.heap, loop_env, value_name, value_slot);
                        locals[value_slot] = entry_value.unwrap_or(Value::Null);
                    }
                    sync_locals_to_env(&mut self.heap, loop_env, &locals);
                    frames[frame_index].env = loop_env;
                    replace_frame_locals(&mut frames[frame_index], &mut local_stack, locals);
                    frames[frame_index].last = Value::Empty;
                }
                RegisterInstruction::ForInContinue { target, span } => {
                    leave_register_for_in_iteration(
                        &mut self.heap,
                        &mut frames[frame_index],
                        &mut local_stack,
                        for_in_stack.last(),
                        span,
                    )?;
                    frames[frame_index].ip = target;
                    continue;
                }
                RegisterInstruction::ForInBreak { target, span } => {
                    leave_register_for_in_iteration(
                        &mut self.heap,
                        &mut frames[frame_index],
                        &mut local_stack,
                        for_in_stack.last(),
                        span,
                    )?;
                    let _ = for_in_stack.pop().ok_or_else(|| {
                        Error::with_kind_span(
                            ErrorKind::Runtime,
                            "for-in state underflow".to_string(),
                            span,
                        )
                    })?;
                    frames[frame_index].ip = target;
                    continue;
                }
                RegisterInstruction::EnterScope { local_count } => {
                    sync_frame_locals_to_env_if_dirty(
                        &mut self.heap,
                        &mut frames[frame_index],
                        &local_stack,
                    );
                    frames[frame_index].env =
                        VmEnv::alloc_child(&mut self.heap, current_env!(), local_count);
                    reset_frame_locals(&mut frames[frame_index], &mut local_stack, local_count);
                }
                RegisterInstruction::ExitScope => {
                    sync_frame_locals_to_env_if_dirty(
                        &mut self.heap,
                        &mut frames[frame_index],
                        &local_stack,
                    );
                    let prev = self
                        .heap
                        .env(current_env!())
                        .prev
                        .expect("scope must have parent");
                    let locals = self.heap.env(prev).slots.clone();
                    frames[frame_index].env = prev;
                    replace_frame_locals(&mut frames[frame_index], &mut local_stack, locals);
                }
                RegisterInstruction::Call {
                    dst,
                    function,
                    args,
                    span,
                    frame_name,
                } => {
                    let function_value =
                        get_register(&frames[frame_index], &register_stack, function, span)?;
                    if let Value::Function(function) = function_value {
                        let function = self.heap.function(function).clone();
                        let name = frame_name
                            .clone()
                            .or_else(|| function.compiled.name.clone())
                            .unwrap_or_else(|| "<anonymous>".to_string());
                        self.push_call_frame(name, span);
                        let reuse_closure_env = can_reuse_closure_env_for_call(&function.compiled);
                        let (call_env, locals) = if reuse_closure_env {
                            if !args.is_empty() {
                                return Err(Error::with_kind(
                                    ErrorKind::Runtime,
                                    "the number of parameter not match".to_string(),
                                ));
                            }
                            (function.env, Vec::new())
                        } else {
                            prepare_register_call_frame_from_registers(
                                &mut self.heap,
                                &function.compiled,
                                function.env,
                                &frames[frame_index],
                                &register_stack,
                                args.as_slice(),
                                span,
                            )?
                        };
                        frames[frame_index].ip += 1;
                        self.record_allocation("frame");
                        let register_base = register_stack.len();
                        let register_count = function.compiled.registers.register_count;
                        let local_base = local_stack.len();
                        let local_count = locals.len();
                        local_stack.extend(locals);
                        register_stack.resize(register_base + register_count, Value::Empty);
                        frames.push(VmFrame {
                            register_base,
                            register_count,
                            local_base,
                            local_count,
                            function: Rc::clone(&function.compiled),
                            env: call_env,
                            locals_synced: reuse_closure_env,
                            upvalues: Rc::clone(&function.upvalues),
                            ip: 0,
                            return_register: Some(dst),
                            last: Value::Empty,
                        });
                        continue;
                    }
                    let args = args
                        .iter()
                        .map(|register| {
                            get_register(&frames[frame_index], &register_stack, *register, span)
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    let value = self.call_builtin_value(function_value, args, span)?;
                    set_register_last(
                        &mut frames[frame_index],
                        &mut register_stack,
                        dst,
                        value,
                        span,
                    )?;
                }
                RegisterInstruction::Return { src } => {
                    let value =
                        get_register(&frames[frame_index], &register_stack, src, zero_span())?;
                    if let Some(flow) = self.finish_current_frame(
                        &mut frames,
                        &mut local_stack,
                        &mut register_stack,
                        VmFlow::Return(value),
                    )? {
                        return Ok(flow);
                    }
                    continue;
                }
                RegisterInstruction::Break { span } => {
                    if let Some(flow) = self.finish_current_frame(
                        &mut frames,
                        &mut local_stack,
                        &mut register_stack,
                        VmFlow::Break(span),
                    )? {
                        return Ok(flow);
                    }
                    continue;
                }
                RegisterInstruction::Continue { span } => {
                    if let Some(flow) = self.finish_current_frame(
                        &mut frames,
                        &mut local_stack,
                        &mut register_stack,
                        VmFlow::Continue(span),
                    )? {
                        return Ok(flow);
                    }
                    continue;
                }
            }
            frames[frame_index].ip += 1;
        }
    }
}
