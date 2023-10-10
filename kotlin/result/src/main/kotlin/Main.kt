import io.github.anuragsoni.result.Result

fun safeDivide(x: Double, y: Double): Result<Double, String> {
    return if (y == 0.0) {
        Result.Error("Divide by zero")
    } else {
        Result.Ok(x/y)
    }
}
fun main() {
    when(val result = safeDivide(121.0, 1.234)) {
        is Result.Ok -> println(result.value)
        is Result.Error -> println(result.value.uppercase())
    }
}
