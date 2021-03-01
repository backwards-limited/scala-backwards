package com.backwards.fp.pancakes

trait Json extends PancakeIngredientCodecs with PancakeStatusCodecs

object Json extends Json