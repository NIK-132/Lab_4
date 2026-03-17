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

/// Вставка значения в бинарное дерево поиска
let rec insert value tree =
    match tree with
    | Empty -> Node(value, Empty, Empty)
    | Node(root, left, right) ->
        if value < root then
            Node(root, insert value left, right)
        else
            Node(root, left, insert value right)

/// Ввод целого положительного числа (глубины) с проверкой
let rec readInt prompt =
    printf "%s" prompt
    let input = Console.ReadLine()
    match Int32.TryParse input with
    | true, value when value > 0 -> value
    | _ ->
        printfn "Ошибка: введите положительное целое число."
        readInt prompt

/// Генерация случайного бинарного дерева поиска с заданным количеством узлов
let rnd = Random()
let generateRandomTree count =
    let rec loop n tree =
        if n <= 0 then tree
        else
            let value = rnd.NextDouble() * 100.0
            loop (n - 1) (insert value tree)
    loop count Empty

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
        printfn "%s%s%d: %.2f" indent side depth v
        printTreeDepth left (depth + 1) "L"
        printTreeDepth right (depth + 1) "R"

[<EntryPoint>]
let main _ =
    let count = readInt "Введите количество узлов в дереве: "

    let originalTree = generateRandomTree count

    printfn "\nИсходное дерево:"
    printTreeDepth originalTree 0 "Root"

    let truncatedTree = originalTree |> treeMap (fun x -> truncate x)

    printfn "\nДерево после отбрасывания дробной части:"
    printTreeDepth truncatedTree 0 "Root"

    0