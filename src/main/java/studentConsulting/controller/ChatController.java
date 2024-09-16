package studentConsulting.controller;

import java.time.LocalDateTime;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.handler.annotation.Payload;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.constant.enums.NotificationStatus;
import studentConsulting.constant.enums.UserType;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.communication.MessageEntity;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.ConversationRepository;
import studentConsulting.repository.MessageRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.INotificationService;
import studentConsulting.service.IUserService;

@RestController
@RequestMapping("${base.url}")
public class ChatController {

    @Autowired
    private SimpMessagingTemplate simpMessagingTemplate;

    @Autowired
    private MessageRepository messageRepository;
    
    @Autowired
    private INotificationService notificationService;  
    
    @Autowired
    private IUserService userService;
    // Xử lý tin nhắn công khai (chat room)
    @MessageMapping("/message")
    public MessageEntity receiveMessage(@Payload MessageEntity message){
        simpMessagingTemplate.convertAndSend("/chatroom/public", message);
        return message;
    }

    @MessageMapping("/private-message")
    public MessageEntity recMessage(@Payload MessageEntity message) {
        System.out.println("Received message payload: " + message);

        // Kiểm tra nếu người gửi hoặc người nhận bị thiếu
        if (message.getSenderName() == null || message.getSenderName().isEmpty() ||
            message.getReceiverName() == null || message.getReceiverName().isEmpty()) {
            throw new ErrorException("Thông tin người gửi hoặc nhận không hợp lệ.");
        }

        // Kiểm tra nếu conversationId bị thiếu
        if (message.getConversation().getId() == null) {
            throw new ErrorException("Conversation ID không hợp lệ.");
        }

        message.setDate(LocalDateTime.now());
        message.setConversation(message.getConversation());

        // Lưu tin nhắn vào cơ sở dữ liệu
        messageRepository.save(message);

        // Gửi tin nhắn đến kênh cá nhân của người nhận
        simpMessagingTemplate.convertAndSendToUser(message.getReceiverName(), "/private", message);

        UserInformationEntity sender = userService.findByFullName(message.getSenderName())
                .orElseThrow(() -> new ErrorException("Không tìm thấy người dùng với tên người gửi: " + message.getSenderName()));

        UserInformationEntity receiver = userService.findByFullName(message.getReceiverName())
                .orElseThrow(() -> new ErrorException("Không tìm thấy người dùng với tên người nhận: " + message.getReceiverName()));

        UserType userType = null;
        if (sender.getAccount().getRole().getName().contains("ROLE_USER")) { 
            userType = UserType.TUVANVIEN;
        } else if (sender.getAccount().getRole().getName().contains("ROLE_TUVANVIEN")) { 
            userType = UserType.USER;
        }
        NotificationEntity notification = NotificationEntity.builder()
                .senderId(sender.getId()) 
                .receiverId(receiver.getId())  
                .content("Bạn có tin nhắn mới từ " + sender.getLastName() + " " + sender.getFirstName())
                .time(LocalDateTime.now())
                .userType(userType) 
                .status(NotificationStatus.UNREAD) 
                .build();

        notificationService.sendNotification(notification);

        return message;
    }


    @MessageMapping("/group-message")
    public MessageEntity receiveGroupMessage(@Payload MessageEntity message) {
        if (message.getConversation().getId() == null) {
            throw new ErrorException("Conversation ID không hợp lệ.");
        }

        message.setDate(LocalDateTime.now());
        messageRepository.save(message);

        simpMessagingTemplate.convertAndSend("/group/" + message.getConversation().getId(), message);

        return message;
    }


    @RequestMapping("/chat/history")
    public ResponseEntity<DataResponse<List<MessageEntity>>> getConversationHistory(@RequestParam Integer conversationId) {
        List<MessageEntity> messages = messageRepository.findByConversationId(conversationId);

        if (messages.isEmpty()) {
            throw new ErrorException("Không tìm thấy tin nhắn nào cho cuộc trò chuyện này");
        }

        DataResponse<List<MessageEntity>> response = DataResponse.<List<MessageEntity>>builder()
            .status("success")
            .message("Lịch sử tin nhắn của cuộc trò chuyện")
            .data(messages)
            .build();

        return ResponseEntity.ok(response);
    }

}
