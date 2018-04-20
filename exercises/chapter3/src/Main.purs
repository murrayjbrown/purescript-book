module Main where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy, null)
import Data.Maybe (Maybe)

import Data.AddressBook


findEntry' :: String -> String -> String -> AddressBook -> Maybe Entry
findEntry' street city state = head <<< filter filterEntry'
  where
    filterEntry' :: Entry -> Boolean
    filterEntry' entry = entry.address.street == street &&
                         entry.address.city == city &&
                         entry.address.state == state

isAddressFound :: String -> String -> AddressBook -> Boolean
isAddressFound f l = not null <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry e = e.firstName == f && e.lastName == l

makeAddress :: String -> String -> String -> Address
makeAddress street city state = { street: street, city: city, state: state }

makeEntry :: String -> String -> Address -> Entry
makeEntry first last addr = { firstName: first, lastName: last, address: addr }

printEntry firstName lastName book = map showEntry (findEntry firstName lastName book)

printBook book = map showEntry book

removeDuplicates :: String -> String -> AddressBook -> AddressBook
removeDuplicates first last abook = rmDups abook
  where
    refEntry = makeEntry first last $ makeAddress "" "" ""
    eqEntry :: Entry -> Entry -> Boolean
    eqEntry a b = a.firstName == b.firstName && a.lastName == b.lastName
    rmDups :: AddressBook -> AddressBook
    rmDups = nubBy (\_ e -> eqEntry e refEntry)


aliceHome = { street: "123 Alice St", city: "Aliceville", state: "Alicia" }
aliceEntry = { firstName: "Alice", lastName: "Smith", address: aliceHome }

bobHome = { street: "123 Bob St", city: "Bobtown", state: "Bobia" }
bobEntry = { firstName:  "Bob", lastName: "Jones", address: bobHome }

carolHome = { street: "123 Carol St", city: "Carolburgh", state: "Carollia" }
carolEntry = { firstName:"Carol", lastName:"Green", address: carolHome }

book = insertEntry aliceEntry $
       insertEntry bobEntry $
       insertEntry aliceEntry $
       insertEntry bobEntry $
       insertEntry carolEntry emptyBook
