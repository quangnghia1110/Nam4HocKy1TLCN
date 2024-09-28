package studentConsulting.service.interfaces.consultant;

import studentConsulting.model.payload.dto.ConsultantStatisticsDTO;

import java.util.List;
import java.util.Map;

public interface IConsultantStatisticsService {
    ConsultantStatisticsDTO getConsultantStatistics(Integer consultantId);

    List<Map<String, Object>> getDeletedQuestionsByYear(Integer consultantId, Integer year);

    List<Map<String, Object>> getAnswersGivenByYear(Integer consultantId, Integer year);

    List<Map<String, Object>> getAnswerApprovalByYear(Integer consultantId, Integer year);

    List<Map<String, Object>> getConsultationSchedulesConsultantByYear(Integer consultantId, Integer year);

    List<Map<String, Object>> getApprovedPostsByYear(Integer consultantId, Integer year);

    List<Map<String, Object>> getConversationsConsultantByYear(Integer consultantId, Integer year);
}
