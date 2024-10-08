package studentConsulting.service.interfaces.advisor;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.entity.question_answer.DeletionLogEntity;
import studentConsulting.model.payload.dto.question_answer.MyQuestionDTO;

import java.time.LocalDate;
import java.util.List;

public interface IAdvisorQuestionService {

    Page<MyQuestionDTO> getDepartmentConsultantsQuestionsFilters(Integer departmentId, String title, String status, LocalDate startDate, LocalDate endDate, Pageable pageable);

    Page<DeletionLogEntity> getDeletionLogsByDepartment(Integer departmentId, Pageable pageable);

    MyQuestionDTO getQuestionByIdAndDepartment(Integer questionId, Integer departmentId);

    void importQuestions(List<List<String>> csvData);

}
