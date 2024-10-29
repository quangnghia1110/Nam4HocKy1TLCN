package studentConsulting.model.payload.mapper.admin;

import org.springframework.stereotype.Component;
import studentConsulting.model.entity.user.RoleAskEntity;
import studentConsulting.model.payload.dto.user.ManageRoleAskDTO;

@Component
public class RoleAskMapper {
    public ManageRoleAskDTO mapToDTO(RoleAskEntity roleAsk) {
        return ManageRoleAskDTO.builder()
                .id(roleAsk.getId())
                .createdAt(roleAsk.getCreatedAt())
                .name(roleAsk.getName())
                .roleId(roleAsk.getRole().getId())
                .build();
    }

}
