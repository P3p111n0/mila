#pragma once

#include <unordered_map>
#include <memory>
#include <string>
#include <optional>

template<typename T>
class ValMap {
  public:
    enum class Scope {
        Global,
        Function,
        Loop,
        Body,
        If
    };
    ValMap() : current_scope(Scope::Global) {}

    std::optional<T> lookup(const std::string & name, Scope top_scope = Scope::Global) const {
        if (data.count(name)) {
            return data.at(name);
        } else if (_parent && current_scope != top_scope) {
            return _parent->lookup(name, top_scope);
        }
        return std::nullopt;
    }

    std::shared_ptr<ValMap<T>> derive(Scope new_scope) {
        std::shared_ptr<ValMap<T>> child = std::make_shared<ValMap<T>>();
        child->_parent = this;
        child->current_scope = new_scope;
        return child;
    }

    std::unordered_map<std::string, T> data;
    Scope current_scope;
  private:
    ValMap<T> * _parent;
};