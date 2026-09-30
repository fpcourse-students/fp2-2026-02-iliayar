-- | Домашка 2. Параметрический полиморфизм: уровень 2.

-- Инстанс Functor для Vec из "Defs" объявляется здесь как задача 2.3, поэтому он orphan.
{-# OPTIONS_GHC -Wno-orphans #-}
module Level2 where

import Data.Kind (Type)
import Defs
import GHC.TypeLits (Symbol)
import MetaUtils (todo)


-- 2.1. Формулы: продвижение вручную
--
-- Поднимите на уровень типов синтаксис пропозициональных формул Prop вручную, без DataKinds
-- и TypeData: объявите пустые типы Var', Not' и типовые операторы ::\/, ::/\, ::->
-- (по одному на конструктор Prop). Пропозициональные переменные — пустые типы A и B.
-- Запишите типом PropExample формулу (a \/ b) -> (~a -> b) ровно с такой расстановкой
-- скобок; приоритеты своим операторам можно не назначать.

data A
data B

-- Здесь ваши объявления Var', Not', ::\/, ::/\ и ::->.
data Var' v
data Not' a
data (::\/) a b
data (::/\) a b
data (::->) a b

infixr 2 ::->
infixl 3 ::/\
infixl 4 ::\/

type PropExample = Var' A ::\/ Var' B ::-> Not' (Var' A) ::-> Var' B


-- 2.2. Формулы: DataKinds
--
-- Запишите ту же формулу типом PropDataExample, используя продвинутые конструкторы Prop
-- и строки уровня типов (кайнд Symbol, переменные "a" и "b"). Припишите PropDataExample
-- точный кайнд явно — сейчас в заглушке стоит неверный.

type PropDataExample :: Prop Symbol
type PropDataExample = Var "a" :\/ Var "b" :-> Not (Var "a") :-> Var "b"


-- 2.3. Функтор
--
-- Реализуйте Functor для вектора. Заметьте, что типы полностью обеспечивают выполнение
-- законов функтора.

instance Functor (Vec n) where
  fmap _ VNil = VNil
  fmap f (VCons x xs) = VCons (f x) $ fmap f xs


-- 2.4. Конкатенация
--
-- Реализуйте vconcat. Длину результата считает функция уровня типов NatPlus — закрытое
-- семейство типов: определение по образцу, как у обычной функции, только на типах.
-- Подробно семейства типов разбираются в главе 3, здесь достаточно прочитать определение.

type family NatPlus (n :: Nat) (m :: Nat) :: Nat where
  NatPlus Zero m = m
  NatPlus (Suc n) m = Suc (NatPlus n m)

vconcat :: Vec n a -> Vec m a -> Vec (NatPlus n m) a
vconcat VNil r = r
vconcat (VCons x xs) r = VCons x $ vconcat xs r


-- 2.5. Гетерогенный zip
--
-- Реализуйте hzip двух гетерогенных списков. Список типов результата считает семейство Zip:
-- определите его по аналогии с NatPlus так, чтобы оно было определено на любых двух списках,
-- в том числе разной длины (лишний хвост отбрасывается, как у обычного zip).

type family Zip (as :: [Type]) (bs :: [Type]) :: [Type] where
  Zip '[] _ = '[]
  Zip _ '[] = '[]
  Zip (a : as) (b : bs) = (a, b) : Zip as bs

hzip :: HList as -> HList bs -> HList (Zip as bs)
hzip HNil _ = HNil
hzip _ HNil = HNil
hzip (HCons x xs) (HCons y ys) = HCons (x, y) $ hzip xs ys


-- 2.6. Полиморфизм в кайндах
--
-- Tagged полиморфен по кайнду тега. Добейтесь Temperature :: TemperatureUnit -> Type -> Type,
-- не используя оператор (::) ни в определении Temperature, ни в его параметрах.
-- Подсказка: посмотрите на кайнд Tagged и вспомните, как передать тип явно.
-- Затем реализуйте c2f: перевод из Цельсия в Фаренгейт, f = c * 1.8 + 32.

newtype Tagged (tag :: k) (a :: Type) = MkTagged a

data TemperatureUnit = Celsius | Fahrenheit | Kelvin

type Temperature tag a = Tagged @TemperatureUnit tag a

c2f :: Temperature Celsius Double -> Temperature Fahrenheit Double
c2f (MkTagged c) = MkTagged $ c * 1.8 + 32


-- 2.7. Числа Чёрча в обёртке
--
-- Тип числа Чёрча полиморфен, поэтому функции над такими числами имеют высший ранг.
-- Обёртка Church прячет квантор под конструктор: снаружи все функции первого ранга,
-- но чтобы построить значение Church, под конструктором приходится написать полиморфную
-- функцию. Реализуйте zero, suc, plus, mult и fromInt (для неотрицательных чисел);
-- toInt дан для проверки.

newtype Church = Church (forall a. (a -> a) -> a -> a)

toInt :: Church -> Int
toInt (Church n) = n (+ 1) 0

zero :: Church
zero = Church $ \_ init -> init

suc :: Church -> Church
suc (Church n) = Church $ \f init -> f $ n f init

plus :: Church -> Church -> Church
plus (Church n) m = n suc m

mult :: Church -> Church -> Church
mult (Church n) m = n (plus m) zero

fromInt :: Int -> Church
fromInt 0 = zero
fromInt n = suc $ fromInt $ n - 1

-- 2.8. Пара Чёрча
--
-- Pair — пара по Чёрчу, синоним с квантором внутри: функция, которая отдаёт обе компоненты
-- любому, кто скажет, что с ними делать. pair и pfst даны; реализуйте psnd и pswap.

type Pair a b = forall c. (a -> b -> c) -> c

pair :: a -> b -> Pair a b
pair x y f = f x y

pfst :: Pair a b -> a
pfst p = p const

psnd :: Pair a b -> b
psnd p = p $ flip const

pswap :: Pair a b -> Pair b a
pswap p = pair (psnd p) (pfst p)
