package studentConsulting.service.interfaces.actor;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.actor.CommonQuestionDTO;
import studentConsulting.model.payload.request.UpdateCommonQuestionRequest;

import java.security.Principal;
import java.time.LocalDate;

public interface ICommonQuestionService {

    CommonQuestionDTO convertToCommonQuestion(Integer questionId, Principal principal);

    CommonQuestionDTO convertToCommonQuestionByDepartment(Integer questionId, Integer departmentId, Principal principal);

    CommonQuestionDTO updateCommonQuestion(Integer commonQuestionId, UpdateCommonQuestionRequest request);

    CommonQuestionDTO updateCommonQuestionByDepartment(Integer commonQuestionId, Integer departmentId, UpdateCommonQuestionRequest request);

    void deleteCommonQuestion(Integer id);

    void deleteCommonQuestionByDepartment(Integer id, Integer departmentId);

    CommonQuestionDTO getCommonQuestionById(Integer questionId);

    CommonQuestionDTO getCommonQuestionByIdAndDepartment(Integer questionId, Integer departmentId);

    public Page<CommonQuestionDTO> getCommonQuestionByRole(Integer departmentId, String title, LocalDate startDate, LocalDate endDate, Pageable pageable);

    public Page<CommonQuestionDTO> getCommonQuestionsWithFilters(Integer departmentId, String title, LocalDate startDate, LocalDate endDate, Pageable pageable);

}
