package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.communication.MessageEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.communication.MessageDTO;
import studentConsulting.repository.communication.MessageRecallRepository;
import studentConsulting.repository.communication.MessageRepository;
import studentConsulting.service.interfaces.admin.IAdminMessageService;
import studentConsulting.specification.communication.MessageSpecification;

import java.util.Collections;

@Service
public class AdminMessageServiceImpl implements IAdminMessageService {

    @Autowired
    private MessageRepository messageRepository;

    @Autowired
    private MessageRecallRepository messageRecallRepository;

    @Override
    @Transactional
    public void deleteMessageById(Integer id) {
        MessageEntity message = messageRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Tin nhắn không tồn tại"));

        message.setRecalledForEveryone(true);
        messageRepository.save(message);
    }

    @Override
    public MessageDTO getMessageById(Integer id) {
        return messageRepository.findById(id)
                .map(message -> mapToDTO(message, message.getSender().getId()))
                .orElseThrow(() -> new ErrorException("Không tìm thấy tin nhắn"));
    }

    @Override
    public Page<MessageDTO> getAllMessagesWithFilters(Integer conversationId, Pageable pageable) {
        Specification<MessageEntity> spec = Specification.where(null);

        if (conversationId != null) {
            spec = spec.and(MessageSpecification.hasConversationId(conversationId));
        }

        return messageRepository.findAll(spec, pageable)
                .map(message -> mapToDTO(message, message.getSender().getId()));
    }

    public MessageDTO mapToDTO(MessageEntity entity, Integer userId) {
        boolean isRecalledBySender = messageRecallRepository.existsByMessageIdAndUserId(entity.getId(), userId);

        String messageContent;
        String imageUrl;
        String fileUrl;

        if (Boolean.TRUE.equals(entity.getRecalledForEveryone()) || isRecalledBySender) {
            messageContent = "Đã thu hồi tin nhắn";
            imageUrl = "Đã thu hồi hình ảnh";
            fileUrl = "Đã thu hồi file";
        } else {
            messageContent = entity.getMessage();
            imageUrl = entity.getImageUrl();
            fileUrl = entity.getFileUrl();
        }

        return MessageDTO.builder()
                .id(entity.getId())
                .conversationId(entity.getConversationId())
                .sender(MessageDTO.UserInformationDTO.builder()
                        .id(entity.getSender().getId())
                        .name(entity.getSender().getName())
                        .avatarUrl(entity.getSender().getAvatarUrl())
                        .build())
                .receiver(entity.getReceiver() != null ?
                        Collections.singletonList(
                                MessageDTO.UserInformationDTO.builder()
                                        .id(entity.getReceiver().getId())
                                        .name(entity.getReceiver().getName())
                                        .avatarUrl(entity.getReceiver().getAvatarUrl())
                                        .build()
                        ) :
                        Collections.emptyList()
                )
                .message(messageContent)
                .imageUrl(imageUrl)
                .fileUrl(fileUrl)
                .date(entity.getDate())
                .messageStatus(entity.getMessageStatus())
                .recalledForEveryone(entity.getRecalledForEveryone())
                .recalledBySender(isRecalledBySender)
                .edited(entity.getEdited())
                .editedDate(entity.getEditedDate())
                .build();
    }
}
