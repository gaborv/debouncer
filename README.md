# Debouncer

A simple package which lets you aggregate a number of incoming messages and trigger some processing only after a delay.<br/>
For basic usage please look at the following code snippet from the sample:


```elm
type Msg
    = EmailUpdated String
    | PhoneUpdated String
    | ValidateEmail String
    | ValidatePhone String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EmailUpdated email ->
            { model
                | email = email
                , emailState = Typing
            }
                ! [ Debouncer.bounce "email" (2 * Time.second) (ValidateEmail email) ]

        PhoneUpdated phone ->
            { model
                | phone = phone
            }
                ! [ Debouncer.bounce "phone" (2 * Time.second) (ValidatePhone phone) ]

        ValidateEmail email ->
            let 
                emailRegex = regex "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\\\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\\\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])"

                emailState = 
                    if contains emailRegex email then
                        Ok
                    else
                        Error
            in
                { model | emailState = emailState } ! [] 

        ValidatePhone phone ->
            Debug.crash "Not implemented"

```
