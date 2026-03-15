// Задание 2. Вариант 7.
// Сформировать список из узлов, являющихся листьями (нет потомков).

open System

/// Структура бинарного дерева
type Tree =
    | Empty
    | Node of float * Tree * Tree

/// Функция свёртки дерева
/// Параметры:
///   nodeF - функция, обрабатывающая узел: принимает значение узла и результаты свёртки левого и правого поддеревьев
///   empty - значение для пустого дерева
///   tree  - исходное дерево
let rec foldTree nodeF empty tree =
    match tree with
    | Empty -> empty
    | Node(v, left, right) ->
        let leftRes = foldTree nodeF empty left
        let rightRes = foldTree nodeF empty right
        nodeF v leftRes rightRes

/// Сбор всех листьев дерева с использованием fold
let leaves tree =
    foldTree (fun v leftList rightList ->
        // Если оба поддерева пусты (их списки пусты), то текущий узел — лист
        if List.isEmpty leftList && List.isEmpty rightList then
            [v]                     // возвращаем список с одним значением
        else
            leftList @ rightList     // иначе объединяем списки потомков
    ) [] tree

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
    let depth = readInt "Введите глубину дерева (положительное целое число): "

    let tree = generateRandomTree depth

    printfn "\nИсходное дерево:"
    printTreeDepth tree 0 "Root"

    let leafList = leaves tree

    printfn "\nСписок листьев:"
    let leafStrings = leafList |> List.map (sprintf "%.2f")
    printfn "%A" leafStrings

    0