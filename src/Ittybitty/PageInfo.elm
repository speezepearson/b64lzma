module Ittybitty.PageInfo exposing (..)

import Ittybitty.Fragments exposing (Fragment)


type alias PageInfo =
  { title : String
  , body : String
  }

type alias EncodingRelation =
    { fragment : Fragment
    , pageInfo : PageInfo
    }
