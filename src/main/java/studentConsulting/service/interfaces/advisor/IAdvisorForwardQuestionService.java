package studentConsulting.service.interfaces.advisor;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.question_answer.ForwardQuestionDTO;
import studentConsulting.model.payload.request.question_answer.UpdateForwardQuestionRequest;

import java.time.LocalDate;
import java.util.List;

public interface IAdvisorForwardQuestionService {

    Page<ForwardQuestionDTO> getForwardQuestionsWithFilters(Integer toDepartmentId, LocalDate startDate, LocalDate endDate, Pageable pageable, Integer departmentId);

    ForwardQuestionDTO updateForwardQuestion(Integer forwardQuestionId, UpdateForwardQuestionRequest forwardQuestionRequest, Integer departmentId);

    void deleteForwardQuestion(Integer forwardQuestionId, Integer departmentId);

    ForwardQuestionDTO getForwardQuestionByIdAndDepartment(Integer forwardQuestionId, Integer departmentId);

    void importForwardQuestions(List<List<String>> csvData);

}
