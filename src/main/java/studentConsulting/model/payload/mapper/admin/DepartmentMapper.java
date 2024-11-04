package studentConsulting.model.payload.mapper.admin;

import org.springframework.stereotype.Component;
import studentConsulting.model.entity.DepartmentEntity;
import studentConsulting.model.payload.dto.manage.ManageDepartmentDTO;

@Component
public class DepartmentMapper {
    public ManageDepartmentDTO mapToDTO(DepartmentEntity department) {
        return ManageDepartmentDTO.builder()
                .id(department.getId())
                .createdAt(department.getCreatedAt())
                .name(department.getName())
                .description(department.getDescription())
                .logo(department.getLogo())
                .build();
    }
}
