export function make(default_value) {
    return { value: default_value }
}

export function get(key) {
    return key.value
}

export function update(key, updater) {
    const new_value = updater(key.value);
    key.value = new_value;
    return new_value;
}
