function deepClone(obj) {
    return JSON.parse(JSON.stringify(obj))
}

module.exports = deepClone;
