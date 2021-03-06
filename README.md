# schemer

![image](https://github.com/jihandong/scheme-interpreter/blob/main/pic/sicp-is-good.jpg)

## scheme十戒
1. 递归时的模式匹配：
   - 递归数字时，应该判断`(zero? n)`和`else`。
   - 递归原子表时，应该判断`(null? lat)`和`else`。
   - 递归S表达式的表时，应该判断`(null? l)`，`(atom? (car l))`，和`else`。
2. 使用`cons`来构造表。
3. 构造表时，先描述第一个元素，再用`cons`把它和递归式连起来。
4. 递归时至少要改变一个参数，比如：
   - 递归数字时，用`(sub1 n)`
   - 递归原子表示时，用`(cdr lat)`。
   - 递归S表达式的表时，如果其非空，表头也非原子，用`(car l)`和`(cdr l)`。

   递归时也要让参数更接近终止元素，比如：
   - 使用`sub1`时，终止条件使用`zero?`。
   - 使用`cdr`时，终止条件使用`null?`。
5. 构造时的终止条件：
   - 用`+`构造表时，应该终止于0。
   - 用`*`构造表时，应该终止于1。
   - 用`cons`构造表时，应该终止于`()`。
6. 等函数正确后，再进行简化。
7. 如果一个对象的成分和自身行为一致，就可以使用递归，比如：
   - 表的子表。
   - 算术表达式的子表达式。
8. 用辅助函数简化表述。
9. 用新函数抽象公共模式。
10. 构造函数时，尽可能一次获取更多的值。

## scheme五律
- `car`：非空表 -> 元素。
- `cdr`：非空表 -> 表。
- `cons`：元素，表 -> 表。
- `null?`：表 -> 布尔值。
- `eq?`：非数原子，非数原子 -> 布尔值。

## SICP Todo List

|No.|Exercises|State|
|:-:|:-|:-:|
|4.1|按特定顺序求值的`list-of-value`|trival|
|4.4|支持`and`和`or`|merged|
|4.5|支持`cond`的特殊用法||
|4.6|支持`let`||
|4.7|支持`let*`||
|4.8|支持`let`的特殊用法||
|4.9|支持`for`，`while`，`do`等迭代结构||
|4.11|frame用“序对的表”而不是“表的序对”表示|trival|
|4.12|用抽象模式重写设置变量，定义变量，查询变量三个函数||
|4.13|尝试定义能删除约束的`make-unbound!`||
