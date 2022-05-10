module test_fortd
    implicit none

contains

    subroutine feed_pets(cats, dogs, food, angry)
        !! Feeds your cats and dogs, if enough food is available. If not enough
        !! food is available, some of your pets will get angry.

        ! Arguments
        integer, intent(in)  :: cats
            !! The number of cats to keep track of.
        integer, intent(in)  :: dogs
            !! The number of dogs to keep track of.
        real, intent(inout)  :: food
            !! The amount of pet food (in kilograms) which you have on hand.
        integer, intent(out) :: angry
            !! The number of pets angry because they weren't fed.

        return
    end subroutine feed_pets
end module test_fortd
