package studentConsulting.model.payload.mapper.actor;

import org.springframework.stereotype.Component;
import studentConsulting.model.entity.CommonQuestionEntity;
import studentConsulting.model.payload.dto.actor.CommonQuestionDTO;

@Component
public class CommonQuestionMapper {
    public CommonQuestionDTO mapToDTO(CommonQuestionEntity question) {
        return CommonQuestionDTO.builder()
                .commonQuestionId(question.getId())
                .department(CommonQuestionDTO.DepartmentDTO.builder()
                        .id(question.getDepartment().getId())
                        .name(question.getDepartment().getName())
                        .build())
                .field(CommonQuestionDTO.FieldDTO.builder()
                        .id(question.getField().getId())
                        .name(question.getField().getName())
                        .build())
                .roleAsk(CommonQuestionDTO.RoleAskDTO.builder()
                        .id(question.getRoleAsk().getId())
                        .name(question.getRoleAsk().getName())
                        .build())
                .title(question.getTitle())
                .content(question.getContent())
                .fileName(question.getFileName())
                .answerTitle(question.getAnswerTitle())
                .answerContent(question.getAnswerContent())
                .answerUserEmail(question.getAnswerUserEmail())
                .answerUserFirstname(question.getUser().getFirstName())
                .answerUserLastname(question.getUser().getLastName())
                .answerCreatedAt(question.getAnswerCreatedAt())
                .views(question.getViews())
                .createdAt(question.getCreatedAt())
                .askerFirstname(question.getUser().getFirstName())
                .askerLastname(question.getUser().getLastName())
                .createdBy(question.getCreatedBy() != null ? question.getCreatedBy().getLastName() + " " + question.getCreatedBy().getFirstName() : "Unknown")
                .build();
    }
}
