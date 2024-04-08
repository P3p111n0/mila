#pragma once

#include <unordered_map>
#include <memory>
#include <string>
#include <optional>

template<typename T>
class ValMap {
  public:
    ValMap() = default;

    std::optional<T> lookup(const std::string & name) const {
        if (data.count(name)) {
            return data.at(name);
        } else if (_parent) {
            return _parent->lookup(name);
        }
        return std::nullopt;
    }

    std::shared_ptr<ValMap<T>> derive() {
        std::shared_ptr<ValMap<T>> child = std::make_shared<ValMap<T>>();
        child->_parent = this;
        return child;
    }

    std::unordered_map<std::string, T> data;
  private:
    ValMap<T> * _parent;
};