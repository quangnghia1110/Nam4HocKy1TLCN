package studentConsulting.service.interfaces.advisor;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.entity.questionAnswer.DeletionLogEntity;
import studentConsulting.model.payload.dto.MyQuestionDTO;

import java.time.LocalDate;

public interface IAdvisorQuestionService {

    Page<MyQuestionDTO> getDepartmentConsultantsQuestionsFilters(Integer departmentId, String title, String status, LocalDate startDate, LocalDate endDate, Pageable pageable);

    Page<DeletionLogEntity> getDeletionLogsByDepartment(Integer departmentId, Pageable pageable);
}
