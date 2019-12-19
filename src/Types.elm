module Types exposing (..)

type alias Fragment = String

type alias PageInfo =
  { title : String
  , body : String
  }

nilPageInfo : PageInfo
nilPageInfo = { title="", body="" }

type alias EncodingRelation =
    { fragment : Fragment
    , pageInfo : PageInfo
    }

nilEncodingRelation : EncodingRelation
nilEncodingRelation =
    { fragment = ""
    , pageInfo = nilPageInfo
    }
