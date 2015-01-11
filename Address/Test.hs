-- Модуль для тестирования библиотеки


import Address.Digit
import Address.Symbol
import Address.Main
import Text.Parsec


main = do

    -- TODO: Тут хорошо было бы сделать модульное тестирование

    {-
    parseTest digitComp "строение 1-A/2"
    parseTest digitComp "стр 1-A/2"
    parseTest digitComp "стр. 1-A/2"
    parseTest digitComp "стр.1-A/2"
    parseTest digitComp "1-A/2 строение"
    parseTest digitComp "1-A/2 стр"

    parseTest symbolComp "ул. 1-я Дубровская"

    parseTest address "д.1 к.2"
    parseTest address "ул. Дубровская д.4 к.2"
    parseTest address "ул. 1-я Дубровская, д.4"
    parseTest address "г. Москва ул. 1-я Дубровская д.4 к.2"

    parseTest address "Раменское г. Высоковольтная ул"
    parseTest address "город москва улица дубровская"
    parseTest address "Егорьевск г. Профсоюзная ул. д. 30" -- Без запятых
    parseTest address "Москва г."
    parseTest address "г Москва"
    parseTest address "Всеволода Вишневского ул., д.4-А"
    parseTest address "ул. Всеволода Вишневского, д.4-А"
    parseTest address "Ореховый б-р, д.49, к.1"
    parseTest address "Семеновская наб., д.3, к.1"
    parseTest address "Алябьева ул.,д.1-33,к.1"
    parseTest address "Алябьева УЛ.,Д.1-33,к.1"
    parseTest address "Зеленоград г., КОРПУС №225-А"

    parseTest address "пр. Ленинский, д.10, к.5"
    -}
    --parseTest symbolComp "д.1, к.2, Ленинский п."
    parseTest address "Ленинский пр, д.10, к.5"
    --parseTest address "ул. 1-я Дубровская, д.4"
