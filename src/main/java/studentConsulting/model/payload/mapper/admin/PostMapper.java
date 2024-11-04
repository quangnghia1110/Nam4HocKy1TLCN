package studentConsulting.model.payload.mapper.admin;

import org.springframework.stereotype.Component;
import studentConsulting.model.entity.PostEntity;
import studentConsulting.model.payload.dto.actor.PostDTO;

@Component
public class PostMapper {
    public PostDTO mapToDTO(PostEntity postEntity) {
        return PostDTO.builder()
                .id(postEntity.getId())
                .title(postEntity.getTitle())
                .content(postEntity.getContent())
                .isAnonymous(postEntity.isAnonymous())
                .userId(postEntity.getUser().getId())
                .name(postEntity.getUser().getLastName() + " " + postEntity.getUser().getFirstName())
                .avatarUrl(postEntity.getUser().getAvatarUrl())
                .fileName(postEntity.getFileName())
                .createdAt(postEntity.getCreatedAt())
                .isApproved(postEntity.isApproved())
                .views(postEntity.getViews())
                .build();
    }
}
