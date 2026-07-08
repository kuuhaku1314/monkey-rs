use std::collections::HashMap;

pub(super) type SymbolId = usize;

#[derive(Default)]
pub(super) struct SymbolInterner {
    names: Vec<String>,
    ids: HashMap<String, SymbolId>,
}

impl SymbolInterner {
    pub(super) fn intern(&mut self, name: &str) -> SymbolId {
        if let Some(id) = self.ids.get(name) {
            return *id;
        }
        let id = self.names.len();
        self.names.push(name.to_string());
        self.ids.insert(name.to_string(), id);
        id
    }

    pub(super) fn name(&self, id: SymbolId) -> &str {
        self.names
            .get(id)
            .map(String::as_str)
            .expect("symbol id must exist")
    }
}
