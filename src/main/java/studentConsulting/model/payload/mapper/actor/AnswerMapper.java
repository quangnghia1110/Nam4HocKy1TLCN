package studentConsulting.model.payload.mapper.actor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import studentConsulting.model.entity.question_answer.AnswerEntity;
import studentConsulting.model.payload.dto.question_answer.AnswerDTO;
import studentConsulting.repository.question_answer.AnswerRepository;

@Component
public class AnswerMapper {
    private final AnswerRepository answerRepository;

    @Autowired
    public AnswerMapper(AnswerRepository answerRepository) {
        this.answerRepository = answerRepository;
    }

    public AnswerDTO mapToAnswerDTO(AnswerEntity answer) {
        return AnswerDTO.builder()
                .answerId(answer.getId())
                .questionId(answer.getQuestion().getId())
                .roleConsultantId(answer.getRoleConsultant().getId())
                .userId(answer.getUser().getId())
                .title(answer.getTitle())
                .content(answer.getContent())
                .file(answer.getFile())
                .createdAt(answer.getCreatedAt())
                .statusApproval(answer.getStatusApproval())
                .statusAnswer(answer.getStatusAnswer())
                .build();
    }
}
