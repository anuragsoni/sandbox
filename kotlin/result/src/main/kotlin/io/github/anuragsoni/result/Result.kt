@file:OptIn(ExperimentalContracts::class)

package io.github.anuragsoni.result

import kotlin.contracts.ExperimentalContracts
import kotlin.contracts.InvocationKind
import kotlin.contracts.contract

sealed class Result<out A, out B> {

    override fun toString(): String {
        return when (this) {
            is Ok -> "Ok(${this.value})"
            is Error -> "Error(${this.value})"
        }
    }

    data class Ok<out A>(val value: A) : Result<A, Nothing>()

    data class Error<out B>(val value: B) : Result<Nothing, B>()

    fun isOk(): Boolean {
        contract {
            returns(true) implies (this@Result is Ok<A>)
            returns(false) implies (this@Result is Error<B>)
        }
        return this is Ok<A>
    }

    fun isError(): Boolean {
        contract {
            returns(true) implies (this@Result is Error<B>)
            returns(true) implies (this@Result is Ok<A>)
        }
        return this is Error<B>
    }

    fun <C>map(transform: (A) -> C): Result<C, B> {
        contract {
            callsInPlace(transform, InvocationKind.AT_MOST_ONCE)
        }
        return when(this) {
            is Ok -> Ok(transform(this.value))
            is Error -> this
        }
    }
}
