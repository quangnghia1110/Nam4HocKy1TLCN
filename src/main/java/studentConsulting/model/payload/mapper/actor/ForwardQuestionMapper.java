package studentConsulting.model.payload.mapper.actor;

import org.springframework.stereotype.Component;
import studentConsulting.model.entity.ForwardQuestionEntity;
import studentConsulting.model.payload.dto.actor.ForwardQuestionDTO;

@Component
public class ForwardQuestionMapper {
    public ForwardQuestionDTO mapToDTO(ForwardQuestionEntity forwardQuestion, Integer consultantId) {
        ForwardQuestionDTO.DepartmentDTO fromDepartmentDTO = ForwardQuestionDTO.DepartmentDTO.builder()
                .id(forwardQuestion.getFromDepartment().getId())
                .name(forwardQuestion.getFromDepartment().getName())
                .build();

        ForwardQuestionDTO.DepartmentDTO toDepartmentDTO = ForwardQuestionDTO.DepartmentDTO.builder()
                .id(forwardQuestion.getToDepartment().getId())
                .name(forwardQuestion.getToDepartment().getName())
                .build();

        ForwardQuestionDTO.ConsultantDTO consultantDTO = ForwardQuestionDTO.ConsultantDTO.builder()
                .id(consultantId)
                .firstName(forwardQuestion.getConsultant().getFirstName())
                .lastName(forwardQuestion.getConsultant().getLastName())
                .build();

        Integer createdBy = (forwardQuestion.getCreatedBy() != null) ? forwardQuestion.getCreatedBy().getId() : null;

        return ForwardQuestionDTO.builder()
                .id(forwardQuestion.getId())
                .title(forwardQuestion.getTitle())
                .fromDepartment(fromDepartmentDTO)
                .toDepartment(toDepartmentDTO)
                .consultant(consultantDTO)
                .statusForward(forwardQuestion.getStatusForward())
                .createdBy(createdBy)
                .build();
    }
}
