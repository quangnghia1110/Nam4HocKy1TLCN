package studentConsulting.model.payload.mapper.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import studentConsulting.model.entity.MessageEntity;
import studentConsulting.model.payload.dto.actor.MessageDTO;
import studentConsulting.repository.actor.MessageRecallRepository;

import java.util.Collections;

@Component
public class MessageMapper {

    private final MessageRecallRepository messageRecallRepository;

    @Autowired
    public MessageMapper(MessageRecallRepository messageRecallRepository) {
        this.messageRecallRepository = messageRecallRepository;
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
