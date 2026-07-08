use std::collections::BTreeMap;
use std::time::Duration;

#[derive(Clone, Default)]
pub struct VmProfile {
    pub total_instructions: u64,
    pub instruction_counts: BTreeMap<&'static str, u64>,
    pub instruction_elapsed: BTreeMap<&'static str, Duration>,
    pub builtin_counts: BTreeMap<String, u64>,
    pub builtin_elapsed: BTreeMap<String, Duration>,
    pub allocation_counts: BTreeMap<String, u64>,
    pub heap_allocated: usize,
    pub call_count: u64,
    pub import_count: u64,
    pub import_cache_hits: u64,
    pub max_register_count: usize,
    pub max_frame_depth: usize,
}

impl VmProfile {
    pub fn sorted_instruction_counts(&self) -> Vec<(&'static str, u64)> {
        let mut counts = self
            .instruction_counts
            .iter()
            .map(|(name, count)| (*name, *count))
            .collect::<Vec<_>>();
        counts.sort_by(|left, right| right.1.cmp(&left.1).then_with(|| left.0.cmp(right.0)));
        counts
    }

    pub fn sorted_builtin_counts(&self) -> Vec<(String, u64)> {
        let mut counts = self
            .builtin_counts
            .iter()
            .map(|(name, count)| (name.clone(), *count))
            .collect::<Vec<_>>();
        counts.sort_by(|left, right| right.1.cmp(&left.1).then_with(|| left.0.cmp(&right.0)));
        counts
    }

    pub fn sorted_instruction_elapsed(&self) -> Vec<(&'static str, Duration)> {
        let mut elapsed = self
            .instruction_elapsed
            .iter()
            .map(|(name, elapsed)| (*name, *elapsed))
            .collect::<Vec<_>>();
        elapsed.sort_by(|left, right| right.1.cmp(&left.1).then_with(|| left.0.cmp(right.0)));
        elapsed
    }

    pub fn sorted_builtin_elapsed(&self) -> Vec<(String, Duration)> {
        let mut elapsed = self
            .builtin_elapsed
            .iter()
            .map(|(name, elapsed)| (name.clone(), *elapsed))
            .collect::<Vec<_>>();
        elapsed.sort_by(|left, right| right.1.cmp(&left.1).then_with(|| left.0.cmp(&right.0)));
        elapsed
    }

    pub fn sorted_allocation_counts(&self) -> Vec<(String, u64)> {
        let mut counts = self
            .allocation_counts
            .iter()
            .map(|(name, count)| (name.clone(), *count))
            .collect::<Vec<_>>();
        counts.sort_by(|left, right| right.1.cmp(&left.1).then_with(|| left.0.cmp(&right.0)));
        counts
    }
}
