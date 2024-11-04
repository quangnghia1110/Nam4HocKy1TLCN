package studentConsulting.model.payload.mapper.admin;

import org.springframework.stereotype.Component;
import studentConsulting.model.entity.RoleConsultantEntity;
import studentConsulting.model.payload.dto.manage.ManageRoleConsultantDTO;

@Component
public class RoleConsultantMapper {
    public ManageRoleConsultantDTO mapToDTO(RoleConsultantEntity roleConsultant) {
        return ManageRoleConsultantDTO.builder()
                .id(roleConsultant.getId())
                .createdAt(roleConsultant.getCreatedAt())
                .name(roleConsultant.getName())
                .roleId(roleConsultant.getRole().getId())
                .build();
    }
}
