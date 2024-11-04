package studentConsulting.service.interfaces.common;

import studentConsulting.model.payload.dto.common.StatisticUserDTO;

import java.util.List;
import java.util.Map;

public interface IStatisticUserService {
    StatisticUserDTO getUserStatistics(Integer userId);

    List<Map<String, Object>> getStatisticsByYear(Integer userId, Integer year);

    List<Map<String, Object>> getRatingsByYear(Integer userId, Integer year);

    List<Map<String, Object>> getConsultationSchedulesByYear(Integer userId, Integer year);

    List<Map<String, Object>> getConversationsByYear(Integer userId, Integer year);

    List<Map<String, Object>> getConversationsMemberByYear(Integer userId, Integer year);
}

