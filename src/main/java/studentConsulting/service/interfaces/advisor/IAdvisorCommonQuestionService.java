package studentConsulting.service.interfaces.advisor;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.question_answer.CommonQuestionDTO;
import studentConsulting.model.payload.request.question_answer.UpdateCommonQuestionRequest;

import java.security.Principal;
import java.time.LocalDate;

public interface IAdvisorCommonQuestionService {

    Page<CommonQuestionDTO> getCommonQuestionsWithFilters(Integer departmentId, String title, LocalDate startDate, LocalDate endDate, Pageable pageable);

    CommonQuestionDTO convertToCommonQuestion(Integer questionId, Principal principal);

    CommonQuestionDTO convertToCommonQuestionByDepartment(Integer questionId, Integer departmentId, Principal principal);

    CommonQuestionDTO updateCommonQuestion(Integer commonQuestionId, UpdateCommonQuestionRequest request);

    CommonQuestionDTO updateCommonQuestionByDepartment(Integer commonQuestionId, Integer departmentId, UpdateCommonQuestionRequest request);

    void deleteCommonQuestion(Integer id);

    void deleteCommonQuestionByDepartment(Integer id, Integer departmentId);

    CommonQuestionDTO getCommonQuestionById(Integer questionId);

    CommonQuestionDTO getCommonQuestionByIdAndDepartment(Integer questionId, Integer departmentId);

    Page<CommonQuestionDTO> getAllCommonQuestionsWithFilters(String title, LocalDate startDate, LocalDate endDate, Pageable pageable);

    Page<CommonQuestionDTO> getCommonQuestionsWithAdvisorFilters(Integer departmentId, String title, LocalDate startDate, LocalDate endDate, Pageable pageable);
}
