package studentConsulting.controller;

import java.security.Principal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

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

import com.cloudinary.Cloudinary;

import studentConsulting.constant.enums.MessageStatus;
import studentConsulting.constant.enums.NotificationContent;
import studentConsulting.constant.enums.NotificationStatus;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.entity.communication.ConversationUserEntity;
import studentConsulting.model.entity.communication.MessageEntity;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.MessageDTO;
import studentConsulting.model.payload.dto.MessageDTO.UserInformationDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.ConversationRepository;
import studentConsulting.repository.ConversationUserRepository;
import studentConsulting.repository.MessageRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.INotificationService;
import studentConsulting.service.IUserService;
import studentConsulting.specification.MessageSpecification;

@RestController
@RequestMapping("${base.url}")
public class ChatController {

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
    private INotificationService notificationService;  
    
    @Autowired
    private IUserService userService;

    @Autowired
	private Cloudinary cloudinary;
    @MessageMapping("/private-message")
    public MessageDTO recMessage(@Payload MessageDTO messageDTO) {
        System.out.println("Received message payload: " + messageDTO);

        if (messageDTO.getSender() == null || messageDTO.getSender().getId() == null) {
            throw new ErrorException("Sender information is missing.");
        }

        UserInformationEntity senderEntity = userService.findById(messageDTO.getSender().getId())
                .orElseThrow(() -> new ErrorException("Không tìm thấy người gửi với ID: " + messageDTO.getSender().getId()));

        if (messageDTO.getReceiver() == null || messageDTO.getReceiver().getId() == null) {
            throw new ErrorException("Receiver information is missing.");
        }

        UserInformationEntity receiverEntity = userService.findById(messageDTO.getReceiver().getId())
                .orElseThrow(() -> new ErrorException("Không tìm thấy người nhận với ID: " + messageDTO.getReceiver().getId()));

        if (messageDTO.getImageUrl() != null) {
            System.out.println("Image URL: " + messageDTO.getImageUrl());
        }

        messageDTO.setSender(MessageDTO.UserInformationDTO.builder()
                .id(senderEntity.getId())
                .name(senderEntity.getLastName() + " " + senderEntity.getFirstName())
                .avatarUrl(senderEntity.getAvatarUrl())
                .build());

        messageDTO.setReceiver(MessageDTO.UserInformationDTO.builder()
                .id(receiverEntity.getId())
                .name(receiverEntity.getLastName() + " " + receiverEntity.getFirstName())
                .avatarUrl(receiverEntity.getAvatarUrl())
                .build());

        messageDTO.setDate(LocalDate.now());
        messageDTO.setMessageStatus(MessageStatus.PRIVATE);

        MessageEntity messageEntity = MessageMapper.toEntity(messageDTO, senderEntity, receiverEntity);
        messageRepository.save(messageEntity);

        simpMessagingTemplate.convertAndSendToUser(String.valueOf(messageDTO.getReceiver().getId()), "/private", messageDTO);

        NotificationType notificationType = receiverEntity.getAccount().getRole().getName().contains("ROLE_TUVANVIEN") 
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

        notificationService.sendNotification(notification);

        return messageDTO;
    }

    @MessageMapping("/group-message")
    public MessageDTO receiveGroupMessage(@Payload MessageDTO messageDTO, Principal principal) {
        if (messageDTO.getConversationId() == null) {
            throw new ErrorException("Conversation ID không hợp lệ.");
        }

        Optional<ConversationEntity> conversationOpt = conversationRepository.findById(messageDTO.getConversationId());
        if (!conversationOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy cuộc trò chuyện.");
        }

        ConversationEntity conversation = conversationOpt.get();

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        UserInformationEntity senderEntity = userOpt.get();

        Optional<ConversationUserEntity> conversationUserOpt = conversationUserRepository.findByConversation_IdAndUser_Id(conversation.getId(), senderEntity.getId());
        if (!conversationUserOpt.isPresent()) {
            throw new ErrorException("Người dùng không có quyền gửi tin nhắn trong cuộc trò chuyện này.");
        }

        UserInformationDTO senderDTO = UserInformationDTO.builder()
                .id(senderEntity.getId())
                .name(senderEntity.getLastName() + " " + senderEntity.getFirstName())
                .avatarUrl(senderEntity.getAvatarUrl())
                .build();
        
        messageDTO.setSender(senderDTO);
        messageDTO.setDate(LocalDate.now());
        messageDTO.setMessageStatus(MessageStatus.PUBLIC);
        
        MessageEntity messageEntity = MessageMapper.toEntity(messageDTO, senderEntity, null);
        messageRepository.save(messageEntity);

        simpMessagingTemplate.convertAndSend("/group/" + conversation.getId(), messageDTO);

        List<ConversationUserEntity> members = conversationUserRepository.findByConversationIdAndExcludeSender(conversation.getId(), senderEntity.getId());
        
        for (ConversationUserEntity member : members) {
            if (!member.getUser().getId().equals(senderEntity.getId())) {
                NotificationEntity notification = NotificationEntity.builder()
                        .senderId(senderEntity.getId()) 
                        .receiverId(member.getUser().getId())  
                        .content(NotificationContent.NEW_CHAT_GROUP + conversation.getName())
                        .time(LocalDateTime.now())
                        .notificationType(NotificationType.GROUP)  
                        .status(NotificationStatus.UNREAD)  
                        .build();

                notificationService.sendNotification(notification);
            }
        }

        return messageDTO;
    }

    @RequestMapping("/chat/history")
    @PreAuthorize("hasRole('USER') or hasRole('TUVANVIEN')")
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

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir),sortBy));

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
            UserInformationDTO senderDTO = UserInformationDTO.builder()
                    .id(entity.getSender().getId())
                    .name(entity.getSender().getName())
                    .build();
            
            UserInformationDTO receiverDTO = UserInformationDTO.builder()
                    .id(entity.getReceiver().getId())
                    .name(entity.getReceiver().getName())
                    .build();
            
            return MessageDTO.builder()
                    .id(entity.getId())
                    .conversationId(entity.getConversationId())
                    .sender(senderDTO)
                    .receiver(receiverDTO)
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
