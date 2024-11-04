package studentConsulting.model.payload.mapper.admin;

import org.springframework.stereotype.Component;
import studentConsulting.model.entity.AccountEntity;
import studentConsulting.model.payload.dto.manage.ManageAccountDTO;

@Component
public class AccountMapper {

    public ManageAccountDTO mapToDTO(AccountEntity accountEntity) {
        return ManageAccountDTO.builder()
                .id(accountEntity.getId())
                .createdAt(accountEntity.getCreatedAt())
                .email(accountEntity.getEmail())
                .isActivity(accountEntity.isActivity())
                .username(accountEntity.getUsername())

                .department(accountEntity.getDepartment() != null ?
                        ManageAccountDTO.DepartmentDTO.builder()
                                .id(accountEntity.getDepartment().getId())
                                .name(accountEntity.getDepartment().getName())
                                .build() : null)

                .role(accountEntity.getRole() != null ?
                        ManageAccountDTO.RoleDTO.builder()
                                .id(accountEntity.getRole().getId())
                                .name(accountEntity.getRole().getName())
                                .build() : null)

                .roleConsultant(accountEntity.getRoleConsultant() != null ?
                        ManageAccountDTO.RoleConsultantDTO.builder()
                                .id(accountEntity.getRoleConsultant().getId())
                                .name(accountEntity.getRoleConsultant().getName())
                                .build() : null)

                .lastActivity(accountEntity.getLastActivity())
                .isOnline(accountEntity.getIsOnline())
                .build();
    }
}
