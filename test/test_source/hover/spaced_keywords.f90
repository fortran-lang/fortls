subroutine spaced_keywords(arg1,  arg2)
    real, dimension (:, :), intent (in) :: arg1
    real, dimension ( size(arg1, 1), maxval([size(arg1, 2), size(arg1, 1)]) ), intent (out) ::  arg2
end subroutine spaced_keywords
