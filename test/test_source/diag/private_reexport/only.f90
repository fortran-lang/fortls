module reexport_only_user
    use reexport_middle, only: renamed_var => shared_var
    implicit none
end module reexport_only_user

module reexport_hidden
    use reexport_base
    implicit none
    private
end module reexport_hidden

module reexport_hidden_user
    use reexport_hidden
    implicit none
end module reexport_hidden_user
