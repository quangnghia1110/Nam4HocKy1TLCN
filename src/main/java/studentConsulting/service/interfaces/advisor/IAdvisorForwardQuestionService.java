package studentConsulting.service.interfaces.advisor;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.question_answer.ForwardQuestionDTO;
import studentConsulting.model.payload.request.question_answer.UpdateForwardQuestionRequest;

import java.time.LocalDate;
import java.util.List;

public interface IAdvisorForwardQuestionService {

    Page<ForwardQuestionDTO> getForwardQuestionByRole(String title, Integer toDepartmentId, LocalDate startDate, LocalDate endDate, Pageable pageable, Integer userId, Integer departmentId, boolean isAdmin, boolean isAdvisor);

    ForwardQuestionDTO updateForwardQuestionByRole(Integer forwardQuestionId, UpdateForwardQuestionRequest forwardQuestionRequest, Integer userId, Integer departmentId, boolean isAdmin, boolean isAdvisor);

    void deleteForwardQuestionByRole(Integer forwardQuestionId, Integer userId, Integer departmentId, boolean isAdmin, boolean isAdvisor);

    ForwardQuestionDTO getForwardQuestionDetailByRole(Integer forwardQuestionId, Integer userId, Integer departmentId, boolean isAdmin, boolean isAdvisor);

    void importForwardQuestions(List<List<String>> csvData);

}
