module AddressBook where

import Prelude
import Data.Maybe
import Data.List
import Control.Alt

type Name = String
type Address = { street :: String, city :: String }
type Person = { name :: Name, address :: Address }
type AddressBook = List Person

addPerson :: AddressBook -> Person -> AddressBook 
addPerson = snoc

hasName n p = p.name == n

findPerson n = (head <<< filter (hasName n)) 
findPerson2 n = (filter (hasName n) >>> head) 

showPerson :: Maybe Person -> String
showPerson Nothing = "nikt"
showPerson (Just p) = p.name

kowalski = {name: "Kowalski", address : { street: "Opolska", city: "Wrocław" } }
nowak = {name: "Nowak", address : { street: "Kaliska", city: "Wrocław" } }

book = addPerson (singleton nowak) kowalski

