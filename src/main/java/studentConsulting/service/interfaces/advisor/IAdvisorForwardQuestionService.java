package studentConsulting.service.interfaces.advisor;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.question_answer.ForwardQuestionDTO;

import java.security.Principal;
import java.time.LocalDate;

public interface IAdvisorForwardQuestionService {
    Page<ForwardQuestionDTO> getForwardQuestionsWithFilters(Integer toDepartmentId, LocalDate startDate, LocalDate endDate, Pageable pageable, Principal principal);
}
