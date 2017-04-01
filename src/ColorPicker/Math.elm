module ColorPicker.Math
    exposing
        ( mod
        , hsvToRgb
        )


mod : Float -> Int -> Float
mod float int =
    let
        remainder =
            float - toFloat (floor float)
    in
        toFloat ((floor float) % int) + remainder


{-| Based on http://www.rapidtables.com/convert/color/hsv-to-rgb.htm
Where
    0 <= h < 360
    0 <= s < <= 1
    0 <= v <= 1
-}
hsvToRgb : Int -> Float -> Float -> ( Int, Int, Int )
hsvToRgb h s v =
    let
        c =
            v * s

        x =
            c * (1 - (abs ((mod (toFloat h / 60) 2) - 1)))

        m =
            v - c

        normalize r =
            round ((r + m) * 255)
    in
        if h < 60 then
            ( normalize c, normalize x, normalize 0 )
        else if h < 120 then
            ( normalize x, normalize c, normalize 0 )
        else if h < 180 then
            ( normalize 0, normalize c, normalize x )
        else if h < 240 then
            ( normalize 0, normalize x, normalize c )
        else if h < 300 then
            ( normalize x, normalize 0, normalize c )
        else
            ( normalize c, normalize 0, normalize x )
