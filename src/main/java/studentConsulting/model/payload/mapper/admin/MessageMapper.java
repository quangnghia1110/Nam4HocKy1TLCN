package studentConsulting.model.payload.mapper.admin;

import org.mapstruct.Context;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;
import studentConsulting.model.entity.MessageEntity;
import studentConsulting.model.payload.dto.actor.MessageDTO;
import studentConsulting.repository.actor.MessageRecallRepository;

import java.util.Collections;
import java.util.List;

@Mapper(componentModel = "spring")
public interface MessageMapper {

    @Mapping(source = "id", target = "id")
    @Mapping(source = "conversationId", target = "conversationId")
    @Mapping(target = "message", source = "entity", qualifiedByName = "getMessageContent")
    @Mapping(target = "imageUrl", source = "entity", qualifiedByName = "getImageUrl")
    @Mapping(target = "fileUrl", source = "entity", qualifiedByName = "getFileUrl")
    @Mapping(source = "date", target = "date")
    @Mapping(source = "messageStatus", target = "messageStatus")
    @Mapping(source = "recalledForEveryone", target = "recalledForEveryone")
    @Mapping(source = "edited", target = "edited")
    @Mapping(source = "editedDate", target = "editedDate")
    @Mapping(target = "recalledBySender", expression = "java(messageRecallRepository.existsByMessageIdAndUserId(entity.getId(), userId))")
    @Mapping(target = "sender", source = "entity", qualifiedByName = "mapSender")
    @Mapping(target = "receiver", source = "entity", qualifiedByName = "mapReceiver")
    MessageDTO mapToDTO(MessageEntity entity, @Context MessageRecallRepository messageRecallRepository, @Context Integer userId);

    @Named("getMessageContent")
    default String getMessageContent(MessageEntity entity, @Context Integer userId, @Context MessageRecallRepository messageRecallRepository) {
        boolean isRecalledBySender = messageRecallRepository.existsByMessageIdAndUserId(entity.getId(), userId);
        if (Boolean.TRUE.equals(entity.getRecalledForEveryone()) || isRecalledBySender) {
            return "Đã thu hồi tin nhắn";
        }
        return entity.getMessage();
    }

    @Named("getImageUrl")
    default String getImageUrl(MessageEntity entity, @Context Integer userId, @Context MessageRecallRepository messageRecallRepository) {
        boolean isRecalledBySender = messageRecallRepository.existsByMessageIdAndUserId(entity.getId(), userId);
        if (Boolean.TRUE.equals(entity.getRecalledForEveryone()) || isRecalledBySender) {
            return "Đã thu hồi hình ảnh";
        }
        return entity.getImageUrl();
    }

    @Named("getFileUrl")
    default String getFileUrl(MessageEntity entity, @Context Integer userId, @Context MessageRecallRepository messageRecallRepository) {
        boolean isRecalledBySender = messageRecallRepository.existsByMessageIdAndUserId(entity.getId(), userId);
        if (Boolean.TRUE.equals(entity.getRecalledForEveryone()) || isRecalledBySender) {
            return "Đã thu hồi file";
        }
        return entity.getFileUrl();
    }

    @Named("mapSender")
    default MessageDTO.UserInformationDTO mapSender(MessageEntity entity) {
        return MessageDTO.UserInformationDTO.builder()
                .id(entity.getSender().getId())
                .name(entity.getSender().getName())
                .avatarUrl(entity.getSender().getAvatarUrl())
                .build();
    }

    @Named("mapReceiver")
    default List<MessageDTO.UserInformationDTO> mapReceiver(MessageEntity entity) {
        if (entity.getReceiver() != null) {
            return Collections.singletonList(
                    MessageDTO.UserInformationDTO.builder()
                            .id(entity.getReceiver().getId())
                            .name(entity.getReceiver().getName())
                            .avatarUrl(entity.getReceiver().getAvatarUrl())
                            .build()
            );
        }
        return Collections.emptyList();
    }
}
