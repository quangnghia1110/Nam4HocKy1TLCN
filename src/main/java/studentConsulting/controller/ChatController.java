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
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.constant.enums.NotificationContent;
import studentConsulting.constant.enums.NotificationStatus;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.entity.communication.ConversationUserEntity;
import studentConsulting.model.entity.communication.MessageEntity;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
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

    @MessageMapping("/private-message")
    public MessageEntity recMessage(@Payload MessageEntity message) {
        System.out.println("Received message payload: " + message);

        // Kiểm tra nếu người gửi hoặc người nhận bị thiếu
        if (message.getSenderName() == null || message.getSenderName().isEmpty() ||
            message.getReceiverName() == null || message.getReceiverName().isEmpty()) {
            throw new ErrorException("Thông tin người gửi hoặc nhận không hợp lệ.");
        }

       

        message.setDate(LocalDate.now());
        message.setConversationId(message.getConversationId());

        // Lưu tin nhắn vào cơ sở dữ liệu
        messageRepository.save(message);

        // Gửi tin nhắn đến kênh cá nhân của người nhận
        simpMessagingTemplate.convertAndSendToUser(message.getReceiverName(), "/private", message);

        UserInformationEntity sender = userService.findByFullName(message.getSenderName())
                .orElseThrow(() -> new ErrorException("Không tìm thấy người dùng với tên người gửi: " + message.getSenderName()));

        UserInformationEntity receiver = userService.findByFullName(message.getReceiverName())
                .orElseThrow(() -> new ErrorException("Không tìm thấy người dùng với tên người nhận: " + message.getReceiverName()));

        NotificationType notificationType = null;
        if (receiver.getAccount().getRole().getName().contains("ROLE_TUVANVIEN")) { 
            notificationType = NotificationType.TUVANVIEN;
        } else if (receiver.getAccount().getRole().getName().contains("ROLE_USER")) { 
            notificationType = NotificationType.USER;
        }
        NotificationEntity notification = NotificationEntity.builder()
                .senderId(sender.getId()) 
                .receiverId(receiver.getId())  
                .content(NotificationContent.NEW_CHAT_PRIVATE.formatMessage(sender.getLastName() + " " + sender.getFirstName()))                .time(LocalDateTime.now())
                .notificationType(notificationType) 
                .status(NotificationStatus.UNREAD) 
                .build();

        notificationService.sendNotification(notification);

        return message;
    }


  

    @MessageMapping("/group-message")
    public MessageEntity receiveGroupMessage(@Payload MessageEntity message) {
        if (message.getConversationId() == null) {
            throw new ErrorException("Conversation ID không hợp lệ.");
        }

        Optional<ConversationEntity> conversationOpt = conversationRepository.findById(message.getConversationId());
        if (!conversationOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy cuộc trò chuyện.");
        }

        ConversationEntity conversation = conversationOpt.get();

        String[] senderNameParts = message.getSenderName().split(" ");
        if (senderNameParts.length < 2) {
            throw new ErrorException("Tên người gửi không hợp lệ. Phải có cả họ và tên.");
        }
        String lastName = senderNameParts[0];  
        String firstName = senderNameParts[1];

        UserInformationEntity sender = userRepository.findByFirstNameAndLastName(firstName, lastName)
                .orElseThrow(() -> new ErrorException("Không tìm thấy người dùng với tên: " + message.getSenderName()));

        Optional<ConversationUserEntity> conversationUserOpt = conversationUserRepository.findByConversation_IdAndUser_Id(conversation.getId(), sender.getId());
        if (!conversationUserOpt.isPresent()) {
            throw new ErrorException("Người dùng không có quyền gửi tin nhắn trong cuộc trò chuyện này.");
        }

        message.setDate(LocalDate.now());
        System.out.println("Saving message: " + message);
        messageRepository.save(message);

        simpMessagingTemplate.convertAndSend("/group/" + conversation.getId(), message);

        List<ConversationUserEntity> members = conversationUserRepository.findByConversationIdAndExcludeSender(conversation.getId(), sender.getId());
        
        for (ConversationUserEntity member : members) {
            if (!member.getUser().getId().equals(sender.getId())) {
                NotificationEntity notification = NotificationEntity.builder()
                        .senderId(sender.getId()) 
                        .receiverId(member.getUser().getId())  
                        .content(NotificationContent.NEW_CHAT_GROUP + conversation.getName())
                        .time(LocalDateTime.now())
                        .notificationType(NotificationType.GROUP)  
                        .status(NotificationStatus.UNREAD)  
                        .build();

                notificationService.sendNotification(notification);
            }
        }

        return message;
    }



    @RequestMapping("/chat/history")
    @PreAuthorize("hasRole('USER') or hasRole('TUVANVIEN')")
    public ResponseEntity<DataResponse<Page<MessageEntity>>> getConversationHistory(
            @RequestParam Integer conversationId,
            Principal principal,  
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "date") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        String currentUsername = principal.getName();
        UserInformationEntity user = userService.findByUsername(currentUsername)
                .orElseThrow(() -> new ErrorException("Người dùng không tồn tại"));

        boolean isMember = conversationUserRepository.existsByConversation_IdAndUser_Id(conversationId, user.getId());

        boolean isOwner = conversationRepository.existsByIdAndUser_Id(conversationId, user.getId());

        if (!isMember && !isOwner) {
            return ResponseEntity.status(HttpStatus.FORBIDDEN)
                    .body(DataResponse.<Page<MessageEntity>>builder()
                            .status("error")
                            .message("Bạn không có quyền truy cập cuộc trò chuyện này.")
                            .build());
        }

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir),sortBy));

        Specification<MessageEntity> spec = Specification.where(MessageSpecification.hasConversationId(conversationId));

        Page<MessageEntity> messages = messageRepository.findAll(spec, pageable);

        if (messages.isEmpty()) {
            throw new ErrorException("Không tìm thấy tin nhắn nào cho cuộc trò chuyện này");
        }

        DataResponse<Page<MessageEntity>> response = DataResponse.<Page<MessageEntity>>builder()
                .status("success")
                .message("Lịch sử tin nhắn của cuộc trò chuyện")
                .data(messages)
                .build();

        return ResponseEntity.ok(response);
    }




}
