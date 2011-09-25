{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

--  ================================================================
--  Copyright (C) 2010 Tim Scheffler
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
--  ================================================================ 
module Controller ( Controller,
                    initController, 
                    getMethodNames,
                    getMethod ) where 

import           Control.Monad
import           Control.Monad.Error
import           Control.Arrow       (first)

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.StablePtr

import           Data.Typeable

import qualified Data.Text           as T
import qualified Data.Map            as M

import           HSObjC
import           Model
import HSTypes

-- The function will be called by the ObjC Controller Proxy
foreign export ccall initController :: Id -> IO Id
foreign export ccall getMethodNames :: Id -> IO Id
foreign export ccall getMethod      :: Id -> Id -> IO Id

initController :: Id -> IO Id
initController outletsId = runId $ 
    do outlets <- fromId outletsId
       model <- liftIO $ newModel

       let contrl = Controller outlets methods model

       connectOutlets contrl
       return $ StableValue contrl
 
getMethodNames :: Id -> IO Id
getMethodNames contrl = runId $
    do contrl' <- (return . wrappedValue) =<< fromId contrl
       return $ M.keys (ctrMethods contrl')

       
getMethod :: Id -> Id -> IO Id       
getMethod contrl mName = runId $
    do contrl' <- (return . wrappedValue) =<< fromId contrl
       mName' <- fromId mName
       case M.lookup mName' (ctrMethods contrl') of
           Nothing  -> throwError $ "Method " ++ (show mName') ++ " not defined."
           -- construct a "bound method" (like Python's) by executing the
           -- (Controller -> IOOBJC StableId) function `act` with the concrete
           -- controller contrl'.
           -- The controller is then available in the resulting StableId via
           -- a closure.
           Just act -> act contrl'

connectOutlets :: Controller -> IOOBJC ()
connectOutlets contrl = do  
  "fh_hello_button" `connect` ff_say_hello
 where
   ff_say_hello :: Action
   ff_say_hello a = do
     msg <- (outlet "fh_hello_field") >>= objectValue
     (outlet "fh_hello_field") >>= setObjectValue (T.toUpper msg)
     
   outlet :: String -> IOOBJC StableId
   outlet oName = case (T.pack oName) `M.lookup` (ctrOutlets contrl) of
                     Just x  -> return x
                     Nothing -> throwError $ "Could not find outlet named " ++ oName

   model :: (Model -> IO a) -> IOOBJC a
   model f = liftIO $ f (ctrModel contrl)

   connect :: String -> Action -> IOOBJC ()
   connect outletName f = outlet outletName >>= makeTarget f 

model' :: Controller -> (Model -> IO a) -> IOOBJC a
model' control f = liftIO $ f (ctrModel control)


           
            
{- "Methods"

    Methods are implemented as functions in which the first argument is
    the object (controller) itself. Just like Python methods.
    In the MethodTable we store functions (Controller -> IOOBJC StablId).
    This way the HSFuncX objects are created only when needed.
-}

rv :: a -> IOOBJC a
rv = return


methods :: MethodTable
methods =  M.fromList $ map ( first T.pack )  methodTable
 where
   methodTable =
     [ ("isCorrectText", mkMethod (\_ -> is_correct))
     , ("give_it", mkMethod (\_ -> darf))
     ] 
     

   mkMethod :: (OBJC a) => (Controller -> a) -> (Controller -> IOOBJC StableId)
   mkMethod action = \contrl ->
     toStableId $ action contrl

   darf :: T.Text -> IO T.Text
   darf = return . T.toUpper
   
   is_correct :: T.Text -> IO T.Text
   is_correct a = do
     putStrLn $ "Got: " ++ (show a)
     return "OH SNAP, SON!"
