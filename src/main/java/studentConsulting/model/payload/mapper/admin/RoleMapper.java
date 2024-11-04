package studentConsulting.model.payload.mapper.admin;

import org.springframework.stereotype.Component;
import studentConsulting.model.entity.RoleEntity;
import studentConsulting.model.payload.dto.actor.RoleDTO;

@Component
public class RoleMapper {
    public RoleDTO mapToDTO(RoleEntity role) {
        return RoleDTO.builder()
                .id(role.getId())
                .name(role.getName())
                .createdAt(role.getCreatedAt())
                .build();
    }
}
