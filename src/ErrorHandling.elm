module ErrorHandling exposing (..)

import ElmCommon exposing (..)
import Html      exposing (Html)
import FP        exposing (maybe, collect, const)

import Task
import Process
import List.Nonempty as N

-- MODEL


type AppErrors = AppErrors (N.Nonempty ErrorNotification)

type ErrorDisplayType = Modal
                      | Inline

type alias ErrorNotification =
  {
    errorDisplay : ErrorDisplayType
  , errorMessage : ErrorMessage
  }

type alias ModalErrors = N.Nonempty ModalError

type ModalError = ModalError ErrorMessage

type InlineError = InlineError ErrorMessage


-- MODEL HELPERS


getInlineError : AppErrors -> Maybe InlineError
getInlineError (AppErrors notifications) =
  List.head <| collect findInlineError (N.toList notifications)

getModalErrors : AppErrors -> Maybe ModalErrors
getModalErrors (AppErrors notifications) =
  N.fromList <| collect findModalError (N.toList notifications)


-- We need collect here, filter + map
findModalError : ErrorNotification -> Maybe ModalError
findModalError errorNotification =
  case errorNotification.errorDisplay of
    Modal  ->  Just <| ModalError errorNotification.errorMessage
    Inline -> Nothing

findInlineError  : ErrorNotification -> Maybe InlineError
findInlineError errorNotification =
  case errorNotification.errorDisplay of
    Modal  -> Nothing
    Inline -> Just <| InlineError errorNotification.errorMessage

addModalError : (a -> Maybe AppErrors) -> (Maybe AppErrors -> a -> a) -> a  -> ErrorMessage -> a
addModalError getter setter model newErrorMessage =
  case getter model of
    (Just appErrors) -> setter (Just <| addModalErrorToAppErrors appErrors newErrorMessage) model
    Nothing          -> setter (Just (AppErrors <| N.fromElement(ErrorNotification Modal newErrorMessage))) model

addInlineError : (a -> Maybe AppErrors) -> (Maybe AppErrors -> a -> a) -> a -> ErrorMessage -> Seconds -> b -> (a, Cmd b)
addInlineError getter setter model newError timeout msg =
  let newModel =
        case getter model of
          -- We currently only support one inline error. This could change in the future
          (Just appErrors) -> setter (Just <| addInlineErrorToAppErrros appErrors newError) model
          Nothing -> setter (Just <| createAppErrorFromInlineError newError) model
  in (newModel, addTimeoutForInlineMessage timeout msg)

addInlineInfo : (Maybe InformationMessage -> a -> a) -> a -> InformationMessage -> Seconds -> b -> (a, Cmd b)
addInlineInfo setter model message timeout msg =
  let newModel = setter (Just message) model
  in (newModel, addTimeoutForInlineMessage timeout msg)


addModalErrorToAppErrors : AppErrors -> ErrorMessage -> AppErrors
addModalErrorToAppErrors (AppErrors notifications) newErrorMessage =
  AppErrors <| N.cons (ErrorNotification Modal newErrorMessage) notifications

addInlineErrorToAppErrros : AppErrors -> ErrorMessage -> AppErrors
addInlineErrorToAppErrros  appErrors newErrorMessage =
  AppErrors <| N.fromElement (ErrorNotification Inline newErrorMessage)

createAppErrorFromInlineError : ErrorMessage -> AppErrors
createAppErrorFromInlineError  newErrorMessage =
  AppErrors <| N.fromElement (ErrorNotification Inline newErrorMessage)

removeInlineErrors : AppErrors -> Maybe AppErrors
removeInlineErrors (AppErrors errors) =
  let result = List.filter (not << isInlineError) (N.toList errors)
  in
    case result of
      []      -> Nothing
      (x::xs) -> Just <| AppErrors (N.Nonempty x xs)

removeModalErrors : AppErrors -> Maybe AppErrors
removeModalErrors (AppErrors errors) =
  let result = List.filter (not << isModalError) (N.toList errors)
  in
    case result of
      []      -> Nothing
      (x::xs) -> Just <| AppErrors (N.Nonempty x xs)

isInlineError : ErrorNotification -> Bool
isInlineError = not << isModalError

isModalError : ErrorNotification -> Bool
isModalError { errorDisplay } =
  case errorDisplay of
    Modal  -> True
    Inline -> False

onErrorModalClosed : (a -> Maybe AppErrors) -> (Maybe AppErrors -> a -> a) -> a -> a
onErrorModalClosed getter setter model =
  case getter model of
    (Just appErrors) ->
      setter (removeModalErrors appErrors) model
    Nothing          -> model

onInlineErrorTimeout : (a -> Maybe AppErrors) -> (Maybe AppErrors -> a -> a) -> a -> a
onInlineErrorTimeout getter setter model =
  case getter model of
    (Just appErrors) ->
      setter (removeInlineErrors appErrors) model
    Nothing          -> model

onInlineInfoTimeout : (a -> Maybe InformationMessage) -> (Maybe InformationMessage -> a -> a) -> a -> a
onInlineInfoTimeout getter setter model =
  case getter model of
    (Just infoMessage) -> setter Nothing model
    Nothing            -> model

getErrors : AppErrors -> (Maybe InlineError, Maybe ModalErrors)
getErrors appErrors =
  let maybeInlineError = getInlineError appErrors
      maybeModalErrors = getModalErrors appErrors
  in (maybeInlineError, maybeModalErrors)


-- VIEW HELPERS


viewInlineError: InlineError -> Html a
viewInlineError  (InlineError errorMessage) = addInlineErrorFlash errorMessage

viewModalErrors : ModalErrors -> a -> Html a
viewModalErrors errorMessages msg =
  openErrorModal (N.map (\(ModalError error) -> error) errorMessages) msg


-- SIDE EFFECTS


addTimeoutForInlineMessage : Seconds -> msg -> Cmd msg
addTimeoutForInlineMessage { seconds } msg =
  let sleepTask = Process.sleep (toFloat <| seconds * 1000)
  in Task.perform (const msg) sleepTask