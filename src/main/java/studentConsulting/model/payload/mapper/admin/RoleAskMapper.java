package studentConsulting.model.payload.mapper.admin;

import org.springframework.stereotype.Component;
import studentConsulting.model.entity.RoleAskEntity;
import studentConsulting.model.payload.dto.manage.ManageRoleAskDTO;

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
