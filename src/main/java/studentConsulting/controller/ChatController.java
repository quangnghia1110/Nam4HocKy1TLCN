package studentConsulting.controller;

import java.time.LocalDateTime;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.handler.annotation.Payload;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.model.entity.communication.MessageEntity;
import studentConsulting.repository.ConversationRepository;
import studentConsulting.repository.MessageRepository;

@RestController
@RequestMapping("/api/chat")
public class ChatController {

    @Autowired
    private SimpMessagingTemplate simpMessagingTemplate;

    @Autowired
    private ConversationRepository conversationRepository;

    @Autowired
    private MessageRepository messageRepository;
    
    // Xử lý tin nhắn công khai (chat room)
    @MessageMapping("/message")
    public MessageEntity receiveMessage(@Payload MessageEntity message){
        simpMessagingTemplate.convertAndSend("/chatroom/public", message);
        return message;
    }

    @MessageMapping("/private-message")
    public MessageEntity recMessage(@Payload MessageEntity message) {
        System.out.println("Received message payload: " + message);

        // Kiểm tra xem các trường senderName và receiverName có rỗng không
        if (message.getSenderName() == null || message.getSenderName().isEmpty() ||
            message.getReceiverName() == null || message.getReceiverName().isEmpty()) {
            throw new RuntimeException("Thông tin người gửi hoặc nhận không hợp lệ.");
        }

        // Kiểm tra nếu conversationId có được gửi đi không
        if (message.getConversationId() == null) {
            throw new RuntimeException("Conversation ID không hợp lệ.");
        }

        // Thiết lập thời gian hiện tại cho tin nhắn
        message.setDate(LocalDateTime.now());

        // Gán conversationId cho tin nhắn
        Integer conversationId = message.getConversationId();
        message.setConversationId(conversationId);

        // Lưu tin nhắn vào cơ sở dữ liệu
        messageRepository.save(message);

        // Gửi tin nhắn đến kênh cá nhân của người nhận
        simpMessagingTemplate.convertAndSendToUser(message.getReceiverName(), "/private", message);
        
        return message;
    }

    // Lấy lịch sử tin nhắn của cuộc trò chuyện
    @RequestMapping("/history/{conversationId}")
    public List<MessageEntity> getConversationHistory(@PathVariable Integer conversationId) {
        // Lấy tất cả tin nhắn thuộc về cuộc trò chuyện này
        return messageRepository.findByConversationId(conversationId);
    }
}
