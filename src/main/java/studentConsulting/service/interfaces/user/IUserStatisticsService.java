package studentConsulting.service.interfaces.user;

import studentConsulting.model.payload.dto.statistic.UserStatisticsDTO;

import java.util.List;
import java.util.Map;

public interface IUserStatisticsService {
    UserStatisticsDTO getUserStatistics(Integer userId);

    List<Map<String, Object>> getStatisticsByYear(Integer userId, Integer year);

    List<Map<String, Object>> getRatingsByYear(Integer userId, Integer year);

    List<Map<String, Object>> getConsultationSchedulesByYear(Integer userId, Integer year);

    List<Map<String, Object>> getConversationsByYear(Integer userId, Integer year);

    List<Map<String, Object>> getConversationsMemberByYear(Integer userId, Integer year);
}

