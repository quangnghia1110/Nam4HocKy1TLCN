package studentConsulting.service.implement;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.entity.communication.MessageEntity;
import studentConsulting.model.exception.CustomFieldErrorException;
import studentConsulting.model.exception.FieldErrorDetail;
import studentConsulting.model.payload.dto.ChatMessageDTO;
import studentConsulting.model.payload.request.socket.ChatMessageRequest;
import studentConsulting.repository.ConversationRepository;
import studentConsulting.repository.MessageRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.IChatService;

@Service
public class ChatServiceImpl implements IChatService {

    @Autowired
    private MessageRepository messageRepository;

    @Autowired
    private ConversationRepository conversationRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private SimpMessagingTemplate messagingTemplate;

//    @Override
//    public ChatMessageDTO saveMessage(ChatMessageRequest messageRequest) {
//        List<FieldErrorDetail> errors = new ArrayList<>();
//
//        // Kiểm tra sự tồn tại của cuộc hội thoại
//        Optional<ConversationEntity> conversationOpt = conversationRepository.findById(messageRequest.getConversationId());
//        if (!conversationOpt.isPresent()) {
//            errors.add(new FieldErrorDetail("conversation", "Cuộc hội thoại không tồn tại"));
//        }
//
//        // Kiểm tra sự tồn tại của người gửi
//        Optional<UserInformationEntity> senderOpt = userRepository.findById(messageRequest.getFromUserId());
//        if (!senderOpt.isPresent()) {
//            errors.add(new FieldErrorDetail("sender", "Người gửi không tồn tại"));
//        }
//
//        // Nếu có lỗi, ném ngoại lệ
//        if (!errors.isEmpty()) {
//            throw new CustomFieldErrorException(errors);
//        }
//
//        ConversationEntity conversation = conversationOpt.get();
//        UserInformationEntity sender = senderOpt.get();
//
//        // Tạo đối tượng MessageEntity
//        MessageEntity messageEntity = MessageEntity.builder()
//                .conversation(conversation)
//                .sender(sender)
//                .messageText(messageRequest.getContent())
//                .sentAt(LocalDate.now())
//                .type(messageRequest.getType())
//                .statusRead(false)
//                .statusSend(true)
//                .statusRecall(false)
//                .build();
//
//        // Lưu tin nhắn vào cơ sở dữ liệu
//        MessageEntity savedMessage = messageRepository.save(messageEntity);
//
//        // Tạo ChatMessageDTO để phản hồi
//        ChatMessageDTO chatMessageDTO = mapToChatMessageDTO(savedMessage);
//
//        // Gửi tin nhắn qua WebSocket (public hoặc private)
//        messagingTemplate.convertAndSend("/topic/public", chatMessageDTO);
//
//        return chatMessageDTO;
//    }

//    @Override
//    public List<ChatMessageDTO> getMessagesByConversationId(Integer conversationId) {
//        List<MessageEntity> messages = messageRepository.findByConversationId(conversationId);
//        
//        // Chuyển đổi các MessageEntity thành ChatMessageDTO
//        return messages.stream()
//                .map(this::mapToChatMessageDTO)
//                .collect(Collectors.toList());
//    }
//    
//    private ChatMessageDTO mapToChatMessageDTO(MessageEntity messageEntity) {
//        return ChatMessageDTO.builder()
//                .conversationId(messageEntity.getConversation().getId())
//                .fromUserId(messageEntity.getSender().getId())
//                .content(messageEntity.getMessageText())
//                .type(messageEntity.getType())
//                .statusRead(messageEntity.getStatusRead())
//                .statusSend(messageEntity.getStatusSend())
//                .statusRecall(messageEntity.getStatusRecall())
//                .sentAt(messageEntity.getSentAt())
//                .build();
//    }
//
//    @Override
//    public MessageEntity findMessageById(Integer id) {
//        return messageRepository.findById(id)
//                .orElseThrow(() -> new RuntimeException("Tin nhắn không tồn tại"));
//    }
}
