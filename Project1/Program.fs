//Задание 1. Вариант 7.
// Дерево содержит вещественные числа. Отбросить в каждом из них дробную часть.

open System

/// Структура бинарного дерева
type Tree =
    | Empty
    | Node of float * Tree * Tree

/// Функция map для дерева: применяет функцию к каждому узлу
let rec treeMap func tree =
    match tree with
    | Empty -> Empty
    | Node(value, left, right) ->
        Node(func value, treeMap func left, treeMap func right)

/// Ввод целого положительного числа (глубины) с проверкой
let rec readInt prompt =
    printf "%s" prompt
    let input = Console.ReadLine()
    match Int32.TryParse input with
    | true, value when value > 0 -> value
    | _ ->
        printfn "Ошибка: введите положительное целое число."
        readInt prompt

/// Генерация случайного дерева заданной глубины (полное бинарное дерево)
let rnd = Random()
let rec generateRandomTree depth =
    if depth <= 0 then
        Empty
    else
        let value = rnd.NextDouble() * 100.0
        Node(value,
             generateRandomTree (depth - 1),
             generateRandomTree (depth - 1))

/// Вывод дерева с указанием глубины и направления.
/// Параметры:
///   tree   - текущее поддерево
///   depth  - текущая глубина (отступ)
///   side   - метка направления ("Root", "L", "R")
let rec printTreeDepth tree depth side =
    match tree with
    | Empty -> ()
    | Node(v, left, right) ->
        let indent = String.replicate depth "  "
        /// выводит отступ, метку, глубину и значение узла с двумя знаками после запятой
        printfn "%s%s%d: %.2f" indent side depth v
        printTreeDepth left (depth + 1) "L"
        printTreeDepth right (depth + 1) "R"

[<EntryPoint>]
let main _ =
    let depth = readInt "Введите глубину дерева (положительное целое число): "

    let originalTree = generateRandomTree depth

    printfn "\nИсходное дерево:"
    printTreeDepth originalTree 0 "Root"

    let truncatedTree = originalTree |> treeMap (fun x -> truncate x)

    printfn "\nДерево после отбрасывания дробной части:"
    printTreeDepth truncatedTree 0 "Root"

    0