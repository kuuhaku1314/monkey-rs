use super::env::{VmEnv, VmEnvHandle};
use super::runtime::value::{
    Value, VmCellHandle, VmFunction, VmFunctionHandle, VmListHandle, VmMapHandle, VmModule,
    VmModuleHandle, VmStringHandle, VmStructInstance, VmStructInstanceHandle, VmStructType,
    VmStructTypeHandle,
};
use crate::map_key::AbleToMapKey;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;

#[derive(Default)]
pub(super) struct VmHeap {
    strings: Vec<String>,
    functions: Vec<VmFunction>,
    modules: Vec<VmModule>,
    lists: Vec<RefCell<Vec<Value>>>,
    maps: Vec<RefCell<HashMap<AbleToMapKey, Value>>>,
    struct_types: Vec<VmStructType>,
    struct_instances: Vec<RefCell<VmStructInstance>>,
    cells: Vec<RefCell<Value>>,
    envs: Vec<VmEnv>,
    allocated: usize,
}

impl VmHeap {
    pub(super) fn allocated(&self) -> usize {
        self.allocated
    }

    pub(super) fn alloc_string(&mut self, value: impl Into<String>) -> Value {
        let handle = VmStringHandle(self.strings.len());
        self.strings.push(value.into());
        self.allocated += 1;
        Value::String(handle)
    }

    pub(super) fn alloc_function(&mut self, function: VmFunction) -> Value {
        let handle = VmFunctionHandle(self.functions.len());
        self.functions.push(function);
        self.allocated += 1;
        Value::Function(handle)
    }

    pub(super) fn alloc_module(&mut self, module: VmModule) -> Value {
        let handle = VmModuleHandle(self.modules.len());
        self.modules.push(module);
        self.allocated += 1;
        Value::Module(handle)
    }

    pub(super) fn alloc_list(&mut self, values: Vec<Value>) -> Value {
        let handle = VmListHandle(self.lists.len());
        self.lists.push(RefCell::new(values));
        self.allocated += 1;
        Value::Slice(handle)
    }

    pub(super) fn alloc_map(&mut self, values: HashMap<AbleToMapKey, Value>) -> Value {
        let handle = VmMapHandle(self.maps.len());
        self.maps.push(RefCell::new(values));
        self.allocated += 1;
        Value::Map(handle)
    }

    pub(super) fn alloc_struct_type(&mut self, struct_type: VmStructType) -> Value {
        let handle = VmStructTypeHandle(self.struct_types.len());
        self.struct_types.push(struct_type);
        self.allocated += 1;
        Value::StructType(handle)
    }

    pub(super) fn alloc_struct_instance(&mut self, instance: VmStructInstance) -> Value {
        let handle = VmStructInstanceHandle(self.struct_instances.len());
        self.struct_instances.push(RefCell::new(instance));
        self.allocated += 1;
        Value::StructInstance(handle)
    }

    pub(super) fn alloc_cell(&mut self, value: Value) -> VmCellHandle {
        let handle = VmCellHandle(self.cells.len());
        self.cells.push(RefCell::new(value));
        self.allocated += 1;
        handle
    }

    pub(super) fn alloc_env(&mut self, env: VmEnv) -> VmEnvHandle {
        let handle = VmEnvHandle(self.envs.len());
        self.envs.push(env);
        self.allocated += 1;
        handle
    }

    pub(super) fn string(&self, handle: VmStringHandle) -> &str {
        self.strings
            .get(handle.0)
            .map(String::as_str)
            .expect("vm string handle must be valid")
    }

    pub(super) fn function(&self, handle: VmFunctionHandle) -> &VmFunction {
        self.functions
            .get(handle.0)
            .expect("vm function handle must be valid")
    }

    pub(super) fn module(&self, handle: VmModuleHandle) -> &VmModule {
        self.modules
            .get(handle.0)
            .expect("vm module handle must be valid")
    }

    pub(super) fn list(&self, handle: VmListHandle) -> Ref<'_, Vec<Value>> {
        self.lists
            .get(handle.0)
            .expect("vm list handle must be valid")
            .borrow()
    }

    pub(super) fn list_mut(&self, handle: VmListHandle) -> RefMut<'_, Vec<Value>> {
        self.lists
            .get(handle.0)
            .expect("vm list handle must be valid")
            .borrow_mut()
    }

    pub(super) fn map(&self, handle: VmMapHandle) -> Ref<'_, HashMap<AbleToMapKey, Value>> {
        self.maps
            .get(handle.0)
            .expect("vm map handle must be valid")
            .borrow()
    }

    pub(super) fn map_mut(&self, handle: VmMapHandle) -> RefMut<'_, HashMap<AbleToMapKey, Value>> {
        self.maps
            .get(handle.0)
            .expect("vm map handle must be valid")
            .borrow_mut()
    }

    pub(super) fn struct_type(&self, handle: VmStructTypeHandle) -> &VmStructType {
        self.struct_types
            .get(handle.0)
            .expect("vm struct type handle must be valid")
    }

    pub(super) fn struct_instance(
        &self,
        handle: VmStructInstanceHandle,
    ) -> Ref<'_, VmStructInstance> {
        self.struct_instances
            .get(handle.0)
            .expect("vm struct instance handle must be valid")
            .borrow()
    }

    pub(super) fn struct_instance_mut(
        &self,
        handle: VmStructInstanceHandle,
    ) -> RefMut<'_, VmStructInstance> {
        self.struct_instances
            .get(handle.0)
            .expect("vm struct instance handle must be valid")
            .borrow_mut()
    }

    pub(super) fn cell(&self, handle: VmCellHandle) -> Value {
        self.cells
            .get(handle.0)
            .expect("vm cell handle must be valid")
            .borrow()
            .clone()
    }

    pub(super) fn set_cell(&self, handle: VmCellHandle, value: Value) {
        *self
            .cells
            .get(handle.0)
            .expect("vm cell handle must be valid")
            .borrow_mut() = value;
    }

    pub(super) fn env(&self, handle: VmEnvHandle) -> &VmEnv {
        self.envs
            .get(handle.0)
            .expect("vm env handle must be valid")
    }

    pub(super) fn env_mut(&mut self, handle: VmEnvHandle) -> &mut VmEnv {
        self.envs
            .get_mut(handle.0)
            .expect("vm env handle must be valid")
    }

    pub(super) fn list_identity(&self, handle: VmListHandle) -> usize {
        handle.0
    }

    pub(super) fn map_identity(&self, handle: VmMapHandle) -> usize {
        self.lists.len() + handle.0
    }

    pub(super) fn struct_instance_identity(&self, handle: VmStructInstanceHandle) -> usize {
        self.lists.len() + self.maps.len() + handle.0
    }
}
