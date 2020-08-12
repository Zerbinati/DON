// Common header of input features of NNUE evaluation function

#ifndef NNUE_FEATURES_COMMON_H_INCLUDED
#define NNUE_FEATURES_COMMON_H_INCLUDED

#include "../../Evaluator.h"
#include "../nnue_common.h"

namespace Evaluator::NNUE::Features {

  class IndexList;

  template <typename... FeatureTypes>
  class FeatureSet;

  // Trigger to perform full calculations instead of difference only
  enum class TriggerEvent {
    kFriendKingMoved // calculate full evaluation when own king moves
  };

  enum class Side {
    kFriend // side to move
  };

}  // namespace Evaluator::NNUE::Features

#endif // #ifndef NNUE_FEATURES_COMMON_H_INCLUDED
