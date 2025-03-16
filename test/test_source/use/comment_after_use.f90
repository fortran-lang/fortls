module dep_mod
    integer :: dep_variable
end module dep_mod

module user_mod
    use dep_mod, only: dep_variable ! disabling comment
end module user_mod
