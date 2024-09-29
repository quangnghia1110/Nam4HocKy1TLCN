package studentConsulting.controller.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.handler.annotation.Payload;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.constant.enums.MessageStatus;
import studentConsulting.constant.enums.NotificationContent;
import studentConsulting.constant.enums.NotificationStatus;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.entity.communication.ConversationUserEntity;
import studentConsulting.model.entity.communication.MessageEntity;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.communication.MessageDTO;
import studentConsulting.model.payload.dto.communication.MessageDTO.UserInformationDTO;
import studentConsulting.model.payload.dto.notification.NotificationResponseDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.communication.ConversationRepository;
import studentConsulting.repository.communication.ConversationUserRepository;
import studentConsulting.repository.communication.MessageRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.common.ICommonNotificationService;
import studentConsulting.service.interfaces.common.ICommonUserService;
import studentConsulting.specification.communication.MessageSpecification;

import java.security.Principal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class CommonChatController {

    @Autowired
    private SimpMessagingTemplate simpMessagingTemplate;

    @Autowired
    private MessageRepository messageRepository;

    @Autowired
    private ConversationUserRepository conversationUserRepository;

    @Autowired
    private ConversationRepository conversationRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ICommonNotificationService notificationService;

    @Autowired
    private ICommonUserService userService;

    @MessageMapping("/private-message")
    public MessageDTO recMessage(@Payload MessageDTO messageDTO) {
        System.out.println("Payload: " + messageDTO);

        if (messageDTO == null || messageDTO.getSender() == null || messageDTO.getSender().getId() == null) {
            throw new ErrorException("Thông tin người gửi bị thiếu.");
        }

        UserInformationEntity senderEntity = userService.findById(messageDTO.getSender().getId())
                .orElseThrow(() -> new ErrorException("Không tìm thấy người gửi với ID: " + messageDTO.getSender().getId()));

        if (messageDTO.getReceiver() == null || messageDTO.getReceiver().isEmpty()) {
            throw new ErrorException("Thông tin người nhận bị thiếu.");
        }

        List<UserInformationEntity> receiverEntities = new ArrayList<>();
        for (UserInformationDTO receiverDTO : messageDTO.getReceiver()) {
            UserInformationEntity receiverEntity = userService.findById(receiverDTO.getId())
                    .orElseThrow(() -> new ErrorException("Không tìm thấy người nhận với ID: " + receiverDTO.getId()));
            receiverEntities.add(receiverEntity);
        }

        if (messageDTO.getImageUrl() != null) {
            System.out.println("Image URL: " + messageDTO.getImageUrl());
        }

        messageDTO.setSender(UserInformationDTO.builder()
                .id(senderEntity.getId())
                .name(senderEntity.getLastName() + " " + senderEntity.getFirstName())
                .avatarUrl(senderEntity.getAvatarUrl())
                .build());

        messageDTO.setReceiver(messageDTO.getReceiver());

        messageDTO.setDate(LocalDateTime.now());
        messageDTO.setMessageStatus(MessageStatus.PRIVATE);

        for (UserInformationEntity receiverEntity : receiverEntities) {
            MessageEntity messageEntity = MessageMapper.toEntity(messageDTO, senderEntity, receiverEntity);
            messageRepository.save(messageEntity);

            simpMessagingTemplate.convertAndSendToUser(String.valueOf(receiverEntity.getId()), "/private", messageDTO);

            NotificationType notificationType = receiverEntity.getAccount().getRole().getName().contains(SecurityConstants.Role.TUVANVIEN)
                    ? NotificationType.TUVANVIEN
                    : NotificationType.USER;

            NotificationEntity notification = NotificationEntity.builder()
                    .senderId(senderEntity.getId())
                    .receiverId(receiverEntity.getId())
                    .content(NotificationContent.NEW_CHAT_PRIVATE.formatMessage(senderEntity.getLastName() + " " + senderEntity.getFirstName()))
                    .time(LocalDateTime.now())
                    .notificationType(notificationType)
                    .status(NotificationStatus.UNREAD)
                    .build();

            NotificationResponseDTO.NotificationDTO notificationDTO = NotificationResponseDTO.NotificationDTO.builder()
                    .senderId(notification.getSenderId())
                    .receiverId(notification.getReceiverId())
                    .content(notification.getContent())
                    .time(notification.getTime())
                    .notificationType(notification.getNotificationType().name())
                    .status(notification.getStatus().name())
                    .build();

            NotificationResponseDTO responseDTO = NotificationResponseDTO.builder()
                    .status("chatNotification")
                    .data(notificationDTO)
                    .build();

            notificationService.sendNotification(notificationDTO);
            System.out.println("Payload: " + responseDTO);
            simpMessagingTemplate.convertAndSendToUser(String.valueOf(receiverEntity.getId()), "/notification", responseDTO);
        }

        return messageDTO;
    }

    @MessageMapping("/group-message")
    public MessageDTO receiveGroupMessage(@Payload MessageDTO messageDTO, Principal principal) {
        System.out.println("Payload: " + messageDTO);

        if (messageDTO == null || messageDTO.getConversationId() == null) {
            throw new ErrorException("Conversation ID không hợp lệ.");
        }

        ConversationEntity conversation = conversationRepository.findById(messageDTO.getConversationId())
                .orElseThrow(() -> new ErrorException("Không tìm thấy cuộc trò chuyện."));

        String email = principal.getName();
        UserInformationEntity senderEntity = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Không tìm thấy người dùng"));

        conversationUserRepository.findByConversation_IdAndUser_Id(conversation.getId(), senderEntity.getId())
                .orElseThrow(() -> new ErrorException("Người dùng không có quyền gửi tin nhắn trong cuộc trò chuyện này."));

        UserInformationDTO senderDTO = UserInformationDTO.builder()
                .id(senderEntity.getId())
                .name(senderEntity.getLastName() + " " + senderEntity.getFirstName())
                .avatarUrl(senderEntity.getAvatarUrl())
                .build();

        List<UserInformationDTO> receivers = new ArrayList<>();
        List<ConversationUserEntity> members = conversationUserRepository.findByConversationIdAndExcludeSender(conversation.getId(), senderEntity.getId());

        members.forEach(member -> {
            if (!member.getUser().getId().equals(senderEntity.getId())) {
                receivers.add(UserInformationDTO.builder()
                        .id(member.getUser().getId())
                        .name(member.getUser().getLastName() + " " + member.getUser().getFirstName())
                        .avatarUrl(member.getUser().getAvatarUrl())
                        .build());
            }
        });

        messageDTO.setSender(senderDTO);
        messageDTO.setDate(LocalDateTime.now());
        messageDTO.setMessageStatus(MessageStatus.PUBLIC);
        messageDTO.setReceivers(receivers);

        MessageEntity messageEntity = MessageMapper.toEntity(messageDTO, senderEntity, null);
        messageRepository.save(messageEntity);

        simpMessagingTemplate.convertAndSend("/group/" + conversation.getId(), messageDTO);

        members.forEach(member -> {
            if (!member.getUser().getId().equals(senderEntity.getId())) {
                NotificationEntity notification = NotificationEntity.builder()
                        .senderId(senderEntity.getId())
                        .receiverId(member.getUser().getId())
                        .content(NotificationContent.NEW_CHAT_GROUP + conversation.getName())
                        .time(LocalDateTime.now())
                        .notificationType(NotificationType.GROUP)
                        .status(NotificationStatus.UNREAD)
                        .build();

                NotificationResponseDTO.NotificationDTO notificationDTO = NotificationResponseDTO.NotificationDTO.builder()
                        .senderId(notification.getSenderId())
                        .receiverId(notification.getReceiverId())
                        .content(notification.getContent())
                        .time(notification.getTime())
                        .notificationType(notification.getNotificationType().name())
                        .status(notification.getStatus().name())
                        .build();

                NotificationResponseDTO responseDTO = NotificationResponseDTO.builder()
                        .status("chatNotification")
                        .data(notificationDTO)
                        .build();

                notificationService.sendNotification(notificationDTO);
                System.out.println("Payload: " + responseDTO);

                simpMessagingTemplate.convertAndSendToUser(String.valueOf(member.getUser().getId()), "/notification", responseDTO);

            }
        });

        return messageDTO;
    }

    @RequestMapping("/chat/history")
    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN)
    public ResponseEntity<DataResponse<Page<MessageDTO>>> getConversationHistory(
            @RequestParam Integer conversationId,
            Principal principal,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "date") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isMember = conversationUserRepository.existsByConversation_IdAndUser_Id(conversationId, user.getId());

        boolean isOwner = conversationRepository.existsByIdAndUser_Id(conversationId, user.getId());

        if (!isMember && !isOwner) {
            return ResponseEntity.status(HttpStatus.FORBIDDEN)
                    .body(DataResponse.<Page<MessageDTO>>builder()
                            .status("error")
                            .message("Bạn không có quyền truy cập cuộc trò chuyện này.")
                            .build());
        }

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Specification<MessageEntity> spec = Specification.where(MessageSpecification.hasConversationId(conversationId));

        Page<MessageEntity> messages = messageRepository.findAll(spec, pageable);

        Page<MessageDTO> messageDTOs = messages.map(MessageMapper::toDTO);

        DataResponse<Page<MessageDTO>> response = DataResponse.<Page<MessageDTO>>builder()
                .status("success")
                .message("Lịch sử tin nhắn của cuộc trò chuyện")
                .data(messageDTOs)
                .build();

        return ResponseEntity.ok(response);
    }

    public static class MessageMapper {

        public static MessageDTO toDTO(MessageEntity entity) {
            return MessageDTO.builder()
                    .id(entity.getId())
                    .conversationId(entity.getConversationId())
                    .sender(UserInformationDTO.builder()
                            .id(entity.getSender().getId())
                            .name(entity.getSender().getName())
                            .avatarUrl(entity.getSender().getAvatarUrl())
                            .build())
                    .receiver(entity.getReceiver() != null ?
                            Collections.singletonList(
                                    UserInformationDTO.builder()
                                            .id(entity.getReceiver().getId())
                                            .name(entity.getReceiver().getName())
                                            .avatarUrl(entity.getReceiver().getAvatarUrl())
                                            .build()
                            ) :
                            Collections.emptyList()
                    )
                    .message(entity.getMessage())
                    .imageUrl(entity.getImageUrl())
                    .date(entity.getDate())
                    .messageStatus(entity.getMessageStatus())
                    .build();
        }

        public static MessageEntity toEntity(MessageDTO dto, UserInformationEntity sender, UserInformationEntity receiver) {
            return MessageEntity.builder()
                    .id(dto.getId())
                    .conversationId(dto.getConversationId())
                    .sender(sender)
                    .receiver(receiver)
                    .message(dto.getMessage())
                    .imageUrl(dto.getImageUrl())
                    .date(dto.getDate())
                    .messageStatus(dto.getMessageStatus())
                    .build();
        }
    }
}
