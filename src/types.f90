module types
    implicit none

    private
    public user_t, message_t

    type :: user_t
        !integer(8) :: id
        character(len=:), allocatable :: id
        character(len=:), allocatable :: username
        character(len=:), allocatable :: discriminator
        character(len=:), allocatable :: global_name
        character(len=:), allocatable :: avatar
        logical :: bot
        logical :: system 
        logical :: mfa_enabled
        character(len=:), allocatable :: banner
        integer(8) :: accent_color
        character(len=:), allocatable :: locale
        logical :: verified
        character(len=:), allocatable :: email
        integer(8) :: flags
        integer(8) :: premium_type
        integer(8) :: public_flags
        character(len=:), allocatable :: avatar_decoration
    end type user_t

    type :: message_t
        !integer(8) :: id
        character(len=:), allocatable :: id
        !integer(8) :: channel_id
        character(len=:), allocatable :: channel_id
        type(user_t), allocatable :: author
        character(len=:), allocatable :: content
        character(len=:), allocatable :: timestamp
        character(len=:), allocatable :: edited_timestamp
        logical :: tts
        logical :: mention_everyone
        ! FIXME: type(user_t) :: mentions(:)
        ! TODO: mention_roles (array of role_t)
        ! TODO: mention_channels (array of channel_mention_t)
        ! TODO: attachments (array of attachment_t)
        ! TODO: embeds (array of embed_t)
        ! TODO: reactions (array of reaction_t)
        ! TODO: nonce (can be int or string, used for message validation)
        logical :: pinned
        integer(8) :: webhook_id
        integer(8) :: type 
        ! TODO: activity (message_activity_t)
        ! TODO: application (partial application_t object) (???)
        integer(8) :: application_id
        ! TODO: message_reference (message_reference_t object)
        integer(8) :: flags
        type(message_t), allocatable :: referenced_message
        ! TODO: interaction (message_interaction_t object)
        ! TODO: thread (channel_t object)
        ! TODO: components (array of message_component_t)
        ! TODO: sticker_items (array of message sticker item objects)
        ! TODO: stickers (array of sticker objects)
        integer(8) :: position
        ! TODO: role_subscription_data (role_subscription_data_t object)
        ! TODO: resolved (resolved data) (?????)
    end type message_t
end module types
