package studentConsulting.model.payload.mapper.admin;

import org.springframework.stereotype.Component;
import studentConsulting.model.entity.department_field.FieldEntity;
import studentConsulting.model.payload.dto.department_field.ManageFieldDTO;

@Component
public class FieldMapper {
    public ManageFieldDTO mapToDTO(FieldEntity field) {
        return ManageFieldDTO.builder()
                .id(field.getId())
                .createdAt(field.getCreatedAt())
                .name(field.getName())
                .departmentId(field.getDepartment() != null ? field.getDepartment().getId() : null)
                .build();
    }
}
