package hmap

interface Key<T : Any> {
    val name: String
}

inline fun <reified T : Any> Key(name: String): Key<T> =
    object : Key<T> {
        override val name: String
            get() = name

        override fun toString(): String {
            return "Key: $name"
        }
    }

class Hmap {
    private val map: MutableMap<Key<*>, Any?> = mutableMapOf()

    @Suppress("UNCHECKED_CAST")
    fun <T : Any> getOrNull(key: Key<T>): T? {
        return map[key] as T?
    }

    operator fun contains(key: Key<*>): Boolean = map.containsKey(key)

    fun <T : Any> put(key: Key<T>, value: T) {
        map[key] = value
    }

    fun <T : Any> remove(key: Key<T>) {
        map.remove(key)
    }

}