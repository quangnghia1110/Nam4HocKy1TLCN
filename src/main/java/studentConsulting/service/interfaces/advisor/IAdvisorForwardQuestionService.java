package studentConsulting.service.interfaces.advisor;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.payload.dto.question_answer.ForwardQuestionDTO;
import studentConsulting.model.payload.request.question_answer.UpdateForwardQuestionRequest;

import java.security.Principal;
import java.time.LocalDate;

public interface IAdvisorForwardQuestionService {
    Page<ForwardQuestionDTO> getForwardQuestionsWithFilters(Integer toDepartmentId, LocalDate startDate, LocalDate endDate, Pageable pageable, Principal principal);

    void deleteForwardQuestion(Integer forwardQuestionId, UserInformationEntity user);

    ForwardQuestionDTO updateForwardQuestion(Integer forwardQuestionId, UpdateForwardQuestionRequest forwardQuestionRequest, UserInformationEntity user);
}
