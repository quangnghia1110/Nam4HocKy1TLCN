package studentConsulting.service.interfaces.advisor;

import studentConsulting.model.payload.dto.statistic.AdvisorStatisticsDTO;

import java.util.List;
import java.util.Map;

public interface IAdvisorStatisticsService {
    AdvisorStatisticsDTO getAdvisorStatistics(Integer advisorId, boolean isAdmin);

    List<Map<String, Object>> getDeletedQuestionsByYear(Integer departmentId, Integer year);

    List<Map<String, Object>> getAnswersGivenByYear(Integer departmentId, Integer year);

    List<Map<String, Object>> getAnswerApprovalByYear(Integer departmentId, Integer year);

    List<Map<String, Object>> getConsultationSchedulesConsultantByYear(Integer departmentId, Integer year);

    List<Map<String, Object>> getConversationsConsultantByYear(Integer departmentId, Integer year);

    List<Map<String, Object>> getRatingsByYear(Integer departmentId, Integer year);

    List<Map<String, Object>> getCommonQuestionsByYear(Integer departmentId, Integer year);
}


