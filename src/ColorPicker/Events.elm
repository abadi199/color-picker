module ColorPicker.Events exposing (onMouseEvent, Point)

import Svg.Events
import Svg
import Json.Decode exposing (map2, field, float, int)


onMouseEvent : (Point -> msg) -> Svg.Attribute msg
onMouseEvent msg =
    mouseEventDecoder
        |> Json.Decode.andThen mouseDragDecoder
        |> Json.Decode.map msg
        |> Svg.Events.on "mousemove"


type alias Point =
    { x : Float, y : Float, button : Int }


mouseEventDecoder : Json.Decode.Decoder Point
mouseEventDecoder =
    Json.Decode.map3 Point
        (field "offsetX" float)
        (field "offsetY" float)
        (field "buttons" int)


mouseDragDecoder : Point -> Json.Decode.Decoder Point
mouseDragDecoder point =
    if point.button == 1 then
        Json.Decode.succeed point
    else
        Json.Decode.fail "button not pressed"
