package studentConsulting.service.interfaces.statistic;

import java.time.LocalDate;
import java.util.List;
import java.util.Map;

public interface IStatisticConsultantAdvisorAdminService {
    public Object getStatisticsByRole(Integer userId, String role, LocalDate startDate, LocalDate endDate);

    public List<Map<String, Object>> getDeletedQuestionsByYear(Integer departmentId, Integer year, Integer consultantId);

    List<Map<String, Object>> getForwardQuestionsByYear(Integer consultantId, Integer departmentId, Integer year);

    public List<Map<String, Object>> getAnswersGivenByRole(Integer consultantId, Integer departmentId, Integer year);

    public List<Map<String, Object>> getAnswerApprovalByRole(Integer consultantId, Integer departmentId, Integer year);

    public List<Map<String, Object>> getConsultationSchedulesByRole(Integer consultantId, Integer departmentId, Integer year);

    public List<Map<String, Object>> getConversationsByRole(Integer consultantId, Integer departmentId, Integer year);

    public List<Map<String, Object>> getRatingsByRole(Integer consultantId, Integer departmentId, Integer year);

    public List<Map<String, Object>> getApprovedAndPendingPostsByYear(Integer consultantId, Integer departmentId, Integer year);

    List<Map<String, Object>> getCommonQuestionsByYear(Integer departmentId, Integer year);

    public List<Map<String, Object>> getUniqueUsersAdvisedByMessages(Integer consultantId, Integer departmentId, Integer year);
}


